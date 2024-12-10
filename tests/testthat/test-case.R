test_that("to_upper and to_lower have equivalent base versions", {
  x <- "This is a sentence."
  expect_identical(str_to_upper(x), toupper(x))
  expect_identical(str_to_lower(x), tolower(x))
})

test_that("to_title creates one capital letter per word", {
  x <- "This is a sentence."
  expect_equal(str_count(x, "\\W+"), str_count(str_to_title(x), "[[:upper:]]"))
})

test_that("to_sentence capitalizes just the first letter", {
  expect_identical(str_to_sentence("a Test"), "A test")
})

test_that("to_pascal converts to pascal case", {
  expect_identical(str_to_pascal("This is a sentence."), "ThisIsASentence")
})

test_that("to_camel converts to camel casee", {
  expect_identical(str_to_camel("This is a sentence."), "thisIsASentence")
})

test_that("to_kebab converts to kebab case", {
  expect_identical(str_to_kebab("This is a sentence."), "this-is-a-sentence")
})

test_that("to_snake converts to snake case", {
  expect_identical(str_to_snake("This is a sentence."), "this_is_a_sentence")
})

test_that("to_snake converts to snake case", {
  expect_identical(str_to_snake("This is a sentence.", separator = "!"), "this!is!a!sentence")
})
