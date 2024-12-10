test_that("str_space works", {
  expect_equal(str_space("XMLHTTPRequestAndHTMLParser"), "XMLHTTPRequest And HTMLParser")
  expect_equal(str_space("XMLHTTPRequestAndHTMLParser", "allcaps"), "X M L H T T P RequestAndH T M L Parser")
  expect_equal(str_space("XMLHTTPRequestAndHTMLParser", "acronyms"), "XMLHTTP Request And HTML Parser")
  expect_equal(str_space("XMLHTTPRequestAndHTMLParser", "everything"), "X M L H T T P R e q u e s t A n d H T M L P a r s e r")
})
