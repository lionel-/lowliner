#' Partial apply a function, filling in some arguments.
#'
#' @description
#' Partial function application allows you to modify a function by pre-filling
#' some of the arguments.  It is particularly useful in conjunction with
#' functionals and other function operators.
#'
#' @details
#' Note that an argument can only be partialised once.
#'
#' @param .f a function. For the output source to read well, this should be a
#'   named function.
#' @param ... named arguments to `.f` that should be partially applied.
#'
#'   Pass an empty `... = ` argument to specify the position of future
#'   arguments relative to partialised ones. See
#'   [rlang::call_modify()] to learn more about this syntax.
#'
#'   These dots support quasiquotation. If you unquote a value, it is
#'   evaluated only once at function creation time.  Otherwise, it is
#'   evaluated each time the function is called.
#' @param .env Soft-deprecated as of purrr 0.3.0. The environments are
#'   now captured via quosures.
#' @param .first Soft-deprecated as of purrr 0.3.0. Please pass an
#'   empty argument `... = ` to specify the position of future
#'   arguments.
#' @param .lazy Soft-deprecated as of purrr 0.3.0. Please unquote the
#'   arguments that should be evaluated once at function creation time.
#'
#' @examples
#' # Partial is designed to replace the use of anonymous functions for
#' # filling in function arguments. Instead of:
#' compact1 <- function(x) discard(x, is.null)
#'
#' # we can write:
#' compact2 <- partial(discard, .p = is.null)
#'
#' # partial() works fine with functions that do non-standard
#' # evaluation
#' my_long_variable <- 1:10
#' plot2 <- partial(plot, my_long_variable)
#' plot2(10:1)
#' plot2(runif(10), type = "l")
#'
#' # Note that you currently can't partialise arguments multiple times:
#' my_mean <- partial(mean, na.rm = TRUE)
#' my_mean <- partial(my_mean, na.rm = FALSE)
#' try(my_mean(1:10))
#'
#'
#' # The evaluation of arguments normally occurs "lazily". Concretely,
#' # this means that arguments are repeatedly evaluated across invocations:
#' f <- partial(runif, n = rpois(1, 5))
#' f
#' f()
#' f()
#'
#' # You can unquote an argument to fix it to a particular value.
#' # Unquoted arguments are evaluated only once when the function is created:
#' f <- partial(runif, n = !!rpois(1, 5))
#' f
#' f()
#' f()
#'
#'
#' # By default, partialised arguments are passed before new ones:
#' my_list <- partial(list, 1, 2)
#' my_list("foo")
#'
#' # Control the position of these arguments by passing an empty
#' # `... = ` argument:
#' my_list <- partial(list, 1, ... = , 2)
#' my_list("foo")
#' @export
partial <- function(.f,
                    ...,
                    .env = NULL,
                    .lazy = NULL,
                    .first = NULL) {
  args <- enquos(...)
  if (has_name(args, "...f")) {
    stop_defunct("`...f` has been renamed to `.f` as of purrr 0.3.0.")
  }

  fn_expr <- enexpr(.f)
  .fn <- switch(typeof(.f),
    builtin = ,
    special =
      as_closure(.f),
    closure =
      .f,
    abort(sprintf("`.f` must be a function, not %s", friendly_type_of(.f)))
  )

  if (!is_null(.env)) {
    signal_soft_deprecated(paste_line(
      "The `.env` argument is soft-deprecated as of purrr 0.3.0.",
    ))
  }
  if (!is_null(.lazy)) {
    signal_soft_deprecated(paste_line(
      "The `.lazy` argument is soft-deprecated as of purrr 0.3.0.",
      "Please unquote the arguments that should be evaluated once and for all.",
      "",
      "  # Before:",
      "  partial(fn, u = runif(1), n = rnorm(1), .lazy = FALSE)",
      "",
      "  # After:",
      "  partial(fn, u = !!runif(1), n = !!rnorm(1))  # All constant",
      "  partial(fn, u = !!runif(1), n = rnorm(1))    # First constant"
    ))
    if (!.lazy) {
      args <- map(args, ~ new_quosure(eval_tidy(.x , env = caller_env()), empty_env()))
    }
  }
  if (!is_null(.first)) {
    signal_soft_deprecated(paste_line(
      "The `.first` argument is soft-deprecated as of purrr 0.3.0.",
      "Please pass a `... =` argument instead.",
      "",
      "  # Before:",
      "  partial(fn, x = 1, y = 2, .first = FALSE)",
      "",
      "  # After:",
      "  partial(fn, x = 1, y = 2, ... = )  # Partialised arguments last",
      "  partial(fn, x = 1, ... = , y = 2)  # Partialised arguments around"
    ))
  }

  env <- caller_env()
  if (!every(args, quo_is_same_env, env)) {
    signal_soft_deprecated(paste_line(
      "All `partial()` arguments must come from the same environment.",
      rlang::format_error_bullets(c(
        i = "This error was likely caused by passing `...` to `partial()` from another function."
      ))))
    return(partial_orig(.fn, fn_expr, args, .first))
  }

  args <- map(args, quo_get_expr)

  # Match arguments in a function where `...` are empty. This way the
  # `... = ` syntax will not interfere with argument matching
  match_args <- function(...) {
    # Use call_modify() to transform `... = ` into `...`
    unmatched_call <- call_modify(call(".fn"), !!!args)
    matched_call <- match.call(.fn, unmatched_call, envir = environment())
    as.list(matched_call)[-1]
  }
  matched_args <- match_args()

  # Remove matched arguments from the formals list of the new function
  fmls <- formals(.fn)
  fmls_index <- !names(fmls) %in% names(matched_args)
  fmls <- fmls[fmls_index]
  fmls_mapping <- fn_fmls_syms(.fn)[fmls_index]

  # The `... = ` syntax indicates the position of future arguments. If
  # supplied, we need to replace it with the formal argument mapping.
  are_dots <- "..." == names(args)
  n_dots <- sum(are_dots)

  if (n_dots == 0) {
    if (is_false(.first)) {
      # Temporary: For compatibility with deprecated `.first`
      args <- vec_c(fmls_mapping, args)
    } else {
      if (is_primitive(.f)) {
        fmls_mapping <- fix_primitive_fmls(args, fmls_mapping)
      }
      args <- vec_c(args, fmls_mapping)
    }
  } else if (n_dots == 1) {
    # Invariant: "..." has the same position in the partition than in
    # `args`
    args_split <- vec_partition(args, vec_locate_runs(are_dots))
    args_split[[which(are_dots)]] <- fmls_mapping
    args <- vec_c(!!!args_split)
  } else {
    abort("Can't supply multiple `... = ` arguments.")
  }

  # Reuse function symbol if possible
  fn_sym <- if (is_symbol(fn_expr)) fn_expr else quote(.fn)

  body <- expr({
    !!fn_sym <- !!.fn
    !!call2(fn_sym, !!!args)
  })

  structure(
    new_function(fmls, body, env = env),
    class = c("purrr_function_partial", "function")
  )
}

#' @export
print.purrr_function_partial <- function(x, ...) {
  cat("<partialised>\n")

  body(x) <- partialised_body(x)
  print(x, ...)
}

# Skip first expression that assigns the function in the execution environment
partialised_body <- function(x) node_car(node_cddr(body(x)))

# Avoid interference from `rlang::as_closure()` arguments
fix_primitive_fmls <- function(args, fmls_args) {
  if (any(names(args) %in% c(".x", ".y"))) {
    redundant <- c("e1", "e2")
  } else {
    redundant <- c(".x", ".y")
  }

  fmls_args[!names(fmls_args) %in% redundant]
}


# Lexical dispatch
#
# We evaluate in the definition environment rather than the caller
# environment in order to support lexically scoped methods. This
# helps with this case:
#
# ```
# local({
#   mean.foobar <- function(...) "foobar"
#   foobar <- structure(list(), class = "foobar")
#
#   mean(foobar) == partial(mean)(foobar)
# })
# ```
#
# These are not standard dispatch semantics, ideally we'd dispatch in
# the caller environment rather than the definition environment. The
# issue is that there's a fundamental conflict between these goals:
#
# (a) Evaluating arguments in their environment (typically def env)
# (b) Allowing substitution of partialised arguments
# (c) Lexical dispatch in caller env rather than def env
#
# It might just be that partialised functions are meant to be private or
# even anonymous (and thus local). Also lexical dispatch in the global
# env should work anyway because most envs inherit from the search
# path. And if in a package, registration will take care of dispatch.
# Let's not worry about this too much.


partial_orig <- function(.fn, fn_expr, args, .first) {
  if (is_false(.first)) {
    # For compatibility
    call <- call_modify(call2(".fn"), ... = , !!!args)
  } else {
    # Pass on `...` from parent function. It should be last, this way if
    # `args` also contain a `...` argument, the position in `args`
    # prevails.
    call <- call_modify(call2(".fn"), !!!args, ... = )
  }

  # Forward caller environment where S3 methods might be defined.
  # See design note below.
  call <- new_quosure(call, caller_env())

  # Unwrap quosured arguments if possible
  call <- quo_invert(call)

  # Derive a mask where dots can be forwarded
  mask <- new_data_mask(env(.fn = .fn))

  partialised <- function(...) {
    mask$... <- environment()$...
    eval_tidy(call, mask)
  }

  structure(
    partialised,
    class = c("purrr_function_partial", "function"),
    body = call,
    fn = fn_expr,
    fmls = alist(... = )
  )
}
