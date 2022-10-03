#' Split up a string into pieces
#'
#' * `str_split()` takes a character vector and returns a list.
#' * `str_split_1()` takes a single string and returns a character vector.
#' * `str_split_fixed()` takes a character vector and returns a matrix.
#' * `str_split_i()` takes a character vector and returns a character
#'   vector.
#'
#' @inheritParams str_detect
#' @inheritParams str_extract
#' @param n Maximum number of pieces to return. Default (Inf) uses all
#'   possible split positions.
#'
#'   For `split_split()`, this determines the maximum length of each element
#'   of the output. For `str_split_fixed()`, this determines the number of
#'   columns in the output; if an input is too short, the result will be padded
#'   with `NA`.
#' @return For `str_split_fixed()`, a character matrix with `n` columns.
#'   For `str_split()`, a list of character vectors.  For `str_split_n()`,
#'   a length `n` character vector.
#' @seealso [stri_split()] for the underlying implementation.
#' @export
#' @examples
#' fruits <- c(
#'   "apples and oranges and pears and bananas",
#'   "pineapples and mangos and guavas"
#' )
#'
#' str_split(fruits, " and ")
#' str_split(fruits, " and ", simplify = TRUE)
#'
#' # If you want to split a single string, use `str_split1`
#' str_split_1(fruits[[1]], " and ")
#'
#' # Specify n to restrict the number of possible matches
#' str_split(fruits, " and ", n = 3)
#' str_split(fruits, " and ", n = 2)
#' # If n greater than number of pieces, no padding occurs
#' str_split(fruits, " and ", n = 5)
#'
#' # Use fixed to return a character matrix
#' str_split_fixed(fruits, " and ", 3)
#' str_split_fixed(fruits, " and ", 4)
#'
#' # str_split_i extracts only a single piece from a string
#' str_split_i(fruits, " and ", 1)
#' str_split_i(fruits, " and ", 3)
str_split <- function(string, pattern, n = Inf, simplify = FALSE) {
  check_lengths(string, pattern)
  check_positive_integer(n)
  check_bool(simplify, allow_na = TRUE)

  if (identical(n, Inf)) {
    n <- -1L
  }

  switch(type(pattern),
    empty = stri_split_boundaries(string, n = n, simplify = simplify, opts_brkiter = opts(pattern)),
    bound = stri_split_boundaries(string, n = n, simplify = simplify, opts_brkiter = opts(pattern)),
    fixed = stri_split_fixed(string, pattern, n = n, simplify = simplify, opts_fixed = opts(pattern)),
    regex = stri_split_regex(string, pattern, n = n, simplify = simplify, opts_regex = opts(pattern)),
    coll  = stri_split_coll(string, pattern, n = n, simplify = simplify, opts_collator = opts(pattern))
  )
}

#' @export
#' @rdname str_split
str_split_1 <- function(string, pattern) {
  check_string(string, allow_empty = TRUE)

  str_split(string, pattern)[[1]]
}

#' @export
#' @rdname str_split
str_split_fixed <- function(string, pattern, n) {
  check_lengths(string, pattern)
  check_positive_integer(n)

  str_split(string, pattern, n = n, simplify = NA)
}

#' @export
#' @rdname str_split
#' @param i Element to return.
str_split_i <- function(string, pattern, i) {
  check_positive_integer(i)

  out <- str_split(string, pattern, simplify = NA, n = i + 1)
  out[, i]
}

check_positive_integer <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!identical(x, Inf)) {
    check_number_whole(x, arg = arg, call = call)
    if (x <= 0) {
      cli::cli_abort("{.arg {arg}} must be a positive integer.", call = call)
    }
  }
}
