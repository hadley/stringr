test_that("patterns coerced to character", {
  x <- factor("a")

  expect_warning(regex(x), "plain character vector")
  expect_warning(coll(x), "plain character vector")
  expect_warning(fixed(x), "plain character vector")
})

test_that("useful error message for bad type", {
  expect_snapshot(error = TRUE, {
    type(1:3)
  })
})

test_that("fallback for regex (#433)", {
  expect_equal(type(structure("x", class = "regex")), "regex")
})

test_that("ignore_case sets strength, but can override manually", {
  x1 <- coll("x", strength = 1)
  x2 <- coll("x", ignore_case = TRUE)
  x3 <- coll("x")

  expect_equal(attr(x1, "options")$strength, 1)
  expect_equal(attr(x2, "options")$strength, 2)
  expect_equal(attr(x3, "options")$strength, 3)
})

test_that("subsetting preserves class", {
  x <- regex("a")
  expect_equal(x[], x)
})
