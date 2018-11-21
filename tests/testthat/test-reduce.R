context("reduce")

test_that("empty input returns init or error", {
  expect_error(reduce(list()), "no `.init` supplied")
  expect_equal(reduce(list(), `+`, .init = 0), 0)
})

test_that("first/value value used as first value", {
  x <- c(1, 1)

  expect_equal(reduce(x, `+`), 2)
  expect_equal(reduce(x, `+`, .init = 1), 3)
  expect_equal(reduce_right(x, `+`), 2)
  expect_equal(reduce_right(x, `+`, .init = 1), 3)
})

test_that("length 1 argument reduced with init", {
  expect_equal(reduce(1, `+`, .init = 1), 2)
  expect_equal(reduce_right(1, `+`, .init = 1), 2)
})

test_that("reduce_right equivalent to reversing input", {
  x <- list(c(2, 1), c(4, 3), c(6, 5))
  expect_equal(reduce_right(x, c), c(6, 5, 4, 3, 2, 1))
  expect_equal(reduce_right(x, c, .init = 7), c(7, 6, 5, 4, 3, 2, 1))
})

test_that("reduce() forwards dots", {
  out <- reduce(1:2, list, "foo", .init = "init")
  exp <- list(list("init", 1L, "foo"), 2L, "foo")
  expect_identical(out, exp)

  out <- reduce(1:2, list, !!!list("foo", "bar"), .init = "init")
  exp <- list(list("init", 1L, "foo", "bar"), 2L, "foo", "bar")
  expect_identical(out, exp)

  out <- reduce2(1:2, 3:4, list, .init = "init", .init := "INIT")
  exp <- list(list("init", 1L, 3L, .init = "INIT"), 2L, 4L, .init = "INIT")
  expect_identical(out, exp)
})

# accumulate --------------------------------------------------------------

test_that("accumulate passes arguments to function", {
  tt <- c("a", "b", "c")
  expect_equal(accumulate(tt, paste, sep = "."), c("a", "a.b", "a.b.c"))
  expect_equal(accumulate_right(tt, paste, sep = "."), c("c.b.a", "c.b", "c"))
})

test_that("accumulate keeps input names", {
  input <- set_names(1:26, letters)
  expect_identical(accumulate(input, sum), set_names(cumsum(1:26), letters))
  expect_identical(accumulate_right(input, sum), set_names(rev(cumsum(rev(1:26))), letters))
})

test_that("accumulate() forwards dots", {
  TRUE
})


# reduce2 -----------------------------------------------------------------

test_that("basic application works", {
  paste2 <- function(x, y, sep) paste(x, y, sep = sep)

  x <- c("a", "b", "c")
  expect_equal(reduce2(x, c("-", "."), paste2), "a-b.c")
  expect_equal(reduce2_right(x, c("-", "."), paste2), "c.b-a")
  expect_equal(reduce2(x, c(".", "-", "."), paste2, .init = "x"), "x.a-b.c")
  expect_equal(reduce2_right(x, c(".", "-", "."), paste2, .init = "x"), "x.c-b.a")
})

test_that("reduce2_right works if lengths match", {
  x <- list(c(0, 1), c(2, 3), c(4, 5))
  y <- list(c(6, 7), c(8, 9))
  expect_equal(reduce2_right(x, y, paste), c("4 2 8 0 6", "5 3 9 1 7"))
  expect_error(reduce2_right(y, x, paste))
})

test_that("reduce returns original input if it was length one", {
  x <- list(c(0, 1), c(2, 3), c(4, 5))
  expect_equal(reduce(x[1], paste), x[[1]])
})

test_that("reduce2() forwards dots", {
  out <- reduce2(1:2, 3:4, list, "foo", .init = "init")
  exp <- list(list("init", 1L, 3L, "foo"), 2L, 4L, "foo")
  expect_identical(out, exp)

  out <- reduce2(1:2, 3:4, list, !!!list("foo", "bar"), .init = "init")
  exp <- list(list("init", 1L, 3L, "foo", "bar"), 2L, 4L, "foo", "bar")
  expect_identical(out, exp)

  out <- reduce2(1:2, 3:4, list, .init = "init", .init := "INIT")
  exp <- list(list("init", 1L, 3L, .init = "INIT"), 2L, 4L, .init = "INIT")
  expect_identical(out, exp)
})
