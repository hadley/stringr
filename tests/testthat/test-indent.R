test_that("indenting works on vectors of different lengths", {
  expect_equal(str_indent(c("why", "video", "cross", "extra", "deal", "authority")),
               c("   why", "   video", "   cross", "   extra", "   deal", "   authority"))

  expect_equal(str_indent(c("why", "video", "cross", "extra", "deal", "authority")),
  c("   why", "   video", "   cross", "   extra", "   deal", "   authority"))
  expect_equal(str_indent("Jan\nFeb\nMar"), "   Jan\n   Feb\n   Mar")
})

test_that("indenting works on vectors of different lengths with optional indent character", {
  expect_equal(str_indent(c("why", "video", "cross", "extra", "deal", "authority"),
                          indent_character = ".", collapse = "\n"), "...why\n...video\n...cross\n...extra\n...deal\n...authority")
  expect_equal(str_indent("Jan\nFeb\nMar", indent_character = "."), "...Jan\n...Feb\n...Mar")
})

test_that("indenting with collapse works", {

  expect_equal(str_indent(c("why", "video", "cross", "extra", "deal", "authority"),
                          indent_character = ".", collapse = "\n"),
               "...why\n...video\n...cross\n...extra\n...deal\n...authority")
})
