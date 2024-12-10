test_that("results are truncated", {
  expect_snapshot(str_view(words))

  # and can control with option
  local_options(stringr.view_n = 5)
  expect_snapshot(str_view(words))
})

test_that("indices come from original vector", {
  expect_snapshot(str_view(letters, "a|z", match = TRUE))
})

test_that("view highlights all matches", {
  x <- c("abc", "def", "fgh")

  expect_snapshot({
    str_view(x, "[aeiou]")
    str_view(x, "d|e")
  })
})

test_that("view highlights whitespace (except a space/nl)", {
  x <- c(" ", "\u00A0", "\n", "\t")
  expect_snapshot({
    str_view(x)

    "or can instead use escapes"
    str_view(x, use_escapes = TRUE)
  })
})

test_that("view displays message for empty vectors", {
  expect_snapshot(str_view(character()))
})

test_that("match argument controls what is shown", {
  x <- c("abc", "def", "fgh", NA)
  out <- str_view(x, "d|e", match = NA)
  expect_length(out, 4)

  out <- str_view(x, "d|e", match = TRUE)
  expect_length(out, 1)

  out <- str_view(x, "d|e", match = FALSE)
  expect_length(out, 3)
})

test_that("can match across lines", {
  local_reproducible_output(crayon = TRUE)
  expect_snapshot(str_view("a\nb\nbbb\nc", "(b|\n)+"))
})

test_that("vectorised over pattern", {
  x <- str_view("a", c("a", "b"), match = NA)
  expect_equal(length(x), 2)
})

test_that("[ preserves class", {
  x <- str_view(letters)
  expect_true(S7_inherits(x[], stringr_view))
})

test_that("str_view_all() is deprecated", {
  expect_snapshot(str_view_all("abc", "a|b"))
})

test_that("html mode continues to work", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("htmlwidgets")

  x <- c("abc", "def", "fgh")
  expect_snapshot({
    str_view(x, "[aeiou]", html = TRUE)$x$html
    str_view(x, "d|e", html = TRUE)$x$html
  })

  # can use escapes
  x <- c(" ", "\u00A0", "\n")
  expect_snapshot({
    str_view(x, html = TRUE, use_escapes = TRUE)$x$html
  })
})
