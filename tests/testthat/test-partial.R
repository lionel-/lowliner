context("partial")

test_that("dots are correctly placed in the signature", {
  out <- partialised_body(partial(runif, n = rpois(1, 5)))
  exp <- expr(runif(n = rpois(1, 5), min = min, max = max))
  expect_identical(out, exp)
})

test_that("no lazy evaluation means arguments aren't repeatedly evaluated", {
  counter <- env(n = 0)
  lazy <- partial(list, n = { counter$n <- counter$n + 1; NULL })
  walk(1:10, ~lazy())
  expect_identical(counter$n, 10)

  counter <- env(n = 0)
  qq <- partial(list, n = !!{ counter$n <- counter$n + 1; NULL })
  walk(1:10, ~qq())
  expect_identical(counter$n, 1)
})

test_that("partial() still works with functions using `missing()`", {
  fn <- function(x) missing(x)
  expect_false(partial(fn, x = 3)())

  fn <- function(x, y) missing(y)
  expect_true(partial(fn)())
  expect_true(partial(fn, x = 1)())
  expect_false(partial(fn, x = 1, y = 2)())
})

test_that("partialised arguments are evaluated in their environments", {
  n <- 0

  partialised <- local({
    n <- 10
    partial(list, n = n)
  })

  expect_identical(partialised(), list(n = 10))
})

test_that("partialised function is evaluated in its environment", {
  fn <- function(...) stop("tilt")

  partialised <- local({
    fn <- function(x) x
    partial(fn, x = "foo")
  })

  expect_identical(partialised(), "foo")
})

test_that("partial() matches argument with primitives", {
  minus <- partial(`-`, .y = 5)
  expect_identical(minus(1), -4)

  minus <- partial(`-`, e2 = 5)
  expect_identical(minus(1), -4)
})

test_that("partial() squashes quosures before printing", {
  expect_known_output(file = test_path("test-partial-print.txt"), {
    foo <- function(x, y) y
    foo <- partial(foo, y = 3)

    # Reproducible environment tag
    environment(foo) <- global_env()

    print(foo)
  })
})

test_that("partial() handles primitives with named arguments after `...`", {
  expect_identical(partial(min, na.rm = TRUE)(1, NA), 1)
  expect_true(is_na(partial(min, na.rm = FALSE)(1, NA)))
})

test_that("partialised function does not infloop when given the same name (#387)", {
  fn <- function(...) "foo"
  fn <- partial(fn)
  expect_identical(fn(), "foo")
})

test_that("partial() handles `... =` arguments", {
  fn <- function(...) list(...)

  default <- partial(fn, "partial")
  expect_identical(default(1), list("partial", 1))

  after <- partial(fn, "partial", ... = )
  expect_identical(after(1), list("partial", 1))

  before <- partial(fn, ... = , "partial")
  expect_identical(before(1), list(1, "partial"))
})

test_that("partial() supports substituted arguments", {
  fn <- function(x) substitute(x)
  fn <- partial(fn, letters)
  expect_identical(fn(), quote(letters))
})

test_that("partial() supports generics (#647)", {
  expect_identical(partial(mean, na.rm = TRUE)(1), 1)

  foo <- TRUE
  expect_identical(partial(mean, na.rm = foo)(1), 1)
})

test_that("partial() supports lexically defined methods in the def env", {
  local({
    mean.purrr__foobar <- function(...) TRUE
    foobar <- structure(list(), class = "purrr__foobar")

    expect_true(partial(mean, na.rm = TRUE)(foobar))
    expect_true(partial(mean, trim = letters, na.rm = TRUE)(foobar))
  })
})

test_that("partial() updates formals list (#681, #690)", {
  fn <- function(x, y) NULL
  expect_identical(formals(partial(fn, x = 1)), pairlist2(y = ))
  expect_identical(formals(partial(fn, y = 1)), pairlist2(x = ))
  expect_identical(formals(partial(fn, x = 1, y = 1)), NULL)


  fn <- function(x = 1, ..., y = 2, z = 3) list(x = x, y = y, z = z, ...)

  out <- partial(fn, ... = )
  expect_identical(formals(out), pairlist2(x = 1, ... = , y = 2, z = 3))
  expect_identical(out("foo"), list(x = "foo", y = 2, z = 3))

  out <- partial(fn, ... = , x = 10)
  expect_identical(formals(out), pairlist2(... = , y = 2, z = 3))
  expect_identical(out("foo"), list(x = 10, y = 2, z = 3, "foo"))

  out <- partial(fn, ... = , x = 10)
  expect_identical(formals(out), pairlist2(... = , y = 2, z = 3))
  expect_identical(out("foo"), list(x = 10, y = 2, z = 3, "foo"))

  out <- partial(fn, x = 10, ... = )
  expect_identical(formals(out), pairlist2(... = , y = 2, z = 3))
  expect_identical(out("foo"), list(x = 10, y = 2, z = 3, "foo"))

  out <- partial(fn, y = 10, ... = , z = 20)
  expect_identical(formals(out), pairlist2(x = 1, ... = ))
  expect_identical(out("foo"), list(x = "foo", y = 10, z = 20))

  out <- partial(fn, x = 10, ... = , z = 20)
  expect_identical(formals(out), pairlist2(... = , y = 2))
  expect_identical(out("foo"), list(x = 10, y = 2, z = 20, "foo"))

  out <- partial(fn, foobar = 10, ... = , quuxbaz = 20)
  expect_identical(formals(out), formals(fn))
  expect_identical(out("foo"), list(x = "foo", y = 2, z = 3, foobar = 10, quuxbaz = 20))

  out <- partial(fn, foobar = 10, ... = , quuxbaz = 20, x = 30)
  expect_identical(formals(out), pairlist2(... = , y = 2, z = 3))
  expect_identical(out("foo"), list(x = 30, y = 2, z = 3, foobar = 10, "foo", quuxbaz = 20))
})

test_that("partial() arguments can refer to default arguments (#743)", {
  my_plus <- function(x, y) x + y

  my_plus_default <- function(x) my_plus(x, y = rep(0, length(x)))
  my_plus_partial <- partial(my_plus, ... = , y = rep(0, length(x)))

  expect_identical(
    my_plus_default(x = 1:10),
    my_plus_partial(x = 1:10)
  )
})


# Life cycle --------------------------------------------------------------

test_that("`.lazy`, `.env`, and `.first` are soft-deprecated", {
  scoped_lifecycle_warnings()
  expect_warning(partial(list, "foo", .lazy = TRUE), "soft-deprecated")
  expect_warning(partial(list, "foo", .env = env()), "soft-deprecated")
  expect_warning(partial(list, "foo", .first = TRUE, "soft-deprecated"))
})

test_that("`.lazy` still works", {
  scoped_options(lifecycle_disable_warnings = TRUE)
  counter <- env(n = 0)
  eager <- partial(list, n = { counter$n <- counter$n + 1; NULL }, .lazy = FALSE)
  walk(1:10, ~eager())
  expect_identical(counter$n, 1)
})

test_that("`.first` still works", {
  scoped_options(lifecycle_disable_warnings = TRUE)
  out <- partialised_body(partial(runif, n = rpois(1, 5), .first = FALSE))
  exp <- expr(runif(min = min, max = max, n = rpois(1, 5)))
  expect_identical(out, exp)

  # partial() also works without partialised arguments
  expect_identical(partialised_body(partial(runif, .first = TRUE)), expr(runif(n = n, min = min, max = max)))
  expect_identical(partialised_body(partial(runif, .first = FALSE)), expr(runif(n = n, min = min, max = max)))
})

test_that("`...f` still works", {
  expect_error(partial(...f = list, x = "foo"), "renamed", class = "defunctError")
})

test_that("partial() still supports quosures", {
  scoped_options(lifecycle_disable_warnings = TRUE)

  arg <- local({
    n <- 0
    quo({ n <<- n + 1; n})
  })

  fn <- partial(list, !!arg)
  expect_identical(fn(), list(1))
  expect_identical(fn(), list(2))
})
