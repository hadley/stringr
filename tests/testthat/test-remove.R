test_that("succesfully wraps str_replace_all", {
  expect_equal(str_remove_all("abababa", "ba"), "a")
  expect_equal(str_remove("abababa", "ba"), "ababa")
})

test_that("successfully dedent str_dedent",{
  expect_equal(str_dedent("  Hello\n    World"), "Hello\n  World")
  expect_equal(str_dedent("  Line 1\n  Line 2\n  Line 3"), "Line 1\nLine 2\nLine 3")
  expect_equal(str_dedent("No indentation"), "No indentation")
  expect_equal(str_dedent(
      "
      this
      is
          a
      test
      "
  ), "\nthis\nis\n    a\ntest\n")
})
