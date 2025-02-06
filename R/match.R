#' Extract components (capturing groups) from a match
#'
#' @description
#' Extract any number of matches defined by unnamed, `(pattern)`, and
#' named, `(?<name>pattern)` capture groups.
#'
#' Use a non-capturing group, `(?:pattern)`, if you need to override default
#' operate precedence but don't want to capture the result.
#'
#' @inheritParams str_detect
#' @param pattern Unlike other stringr functions, `str_match()` only supports
#'   regular expressions, as described `vignette("regular-expressions")`.
#'   The pattern should contain at least one capturing group.
#' @return
#' * `str_match()`: a character matrix with the same number of rows as the
#'   length of `string`/`pattern`. The first column is the complete match,
#'   followed by one column for each capture group. The columns will be named
#'   if you used "named captured groups", i.e. `(?<name>pattern')`.
#'
#' * `str_match_all()`: a list of the same length as `string`/`pattern`
#'   containing character matrices. Each matrix has columns as described above
#'   and one row for each match.
#'
#' @seealso [str_extract()] to extract the complete match,
#'   [stringi::stri_match()] for the underlying implementation.
#' @export
#' @examples
#' strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
#'   "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
#'   "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
#'   "Home: 543.355.3679")
#' phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
#'
#' str_extract(strings, phone)
#' str_match(strings, phone)
#'
#' # Extract/match all
#' str_extract_all(strings, phone)
#' str_match_all(strings, phone)
#'
#' # You can also name the groups to make further manipulation easier
#' phone <- "(?<area>[2-9][0-9]{2})[- .](?<phone>[0-9]{3}[- .][0-9]{4})"
#' str_match(strings, phone)
#'
#' x <- c("<a> <b>", "<a> <>", "<a>", "", NA)
#' str_match(x, "<(.*?)> <(.*?)>")
#' str_match_all(x, "<(.*?)>")
#'
#' str_extract(x, "<.*?>")
#' str_extract_all(x, "<.*?>")
str_match <- function(string, pattern) {
  check_lengths(string, pattern)
  if (type(pattern) != "regex") {
    cli::cli_abort(tr_("{.arg pattern} must be a regular expression."))
  }

  stri_match_first_regex(string,
    pattern,
    opts_regex = opts(pattern)
  )
}

#' @rdname str_match
#' @export
str_match_all <- function(string, pattern) {
  check_lengths(string, pattern)
  if (type(pattern) != "regex") {
    cli::cli_abort(tr_("{.arg pattern} must be a regular expression."))
  }

  stri_match_all_regex(string,
    pattern,
    omit_no_match = TRUE,
    opts_regex = opts(pattern)
  )
}
