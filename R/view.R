#' View strings and matches
#'
#' @description
#' `str_view()` is used to print the underlying representation of a string and
#' to see how a `pattern` matches.
#'
#' Matches are surrounded by `<>` and unusual whitespace (i.e. all whitespace
#' apart from `" "` and `"\n"`) are surrounded by `{}` and escaped. Where
#' possible, matches and unusual whitespace are coloured blue and `NA`s red.
#'
#' @inheritParams str_detect
#' @param match If `pattern` is supplied, which elements should be shown?
#'
#'   * `TRUE`, the default, shows only elements that match the pattern.
#'   * `NA` shows all elements.
#'   * `FALSE` shows only elements that don't match the pattern.
#'
#'   If `pattern` is not supplied, all elements are always shown.
#' @param html Use HTML output? If `TRUE` will create an HTML widget; if `FALSE`
#'   will style using ANSI escapes.
#' @param use_escapes If `TRUE`, all non-ASCII characters will be rendered
#'   with unicode escapes. This is useful to see exactly what underlying
#'   values are stored in the string.
#' @export
#' @examples
#' # Show special characters
#' str_view(c("\"\\", "\\\\\\", "fgh", NA, "NA"))
#'
#' # A non-breaking space looks like a regular space:
#' nbsp <- "Hi\u00A0you"
#' nbsp
#' # But it doesn't behave like one:
#' str_detect(nbsp, " ")
#' # So str_view() brings it to your attention with a blue background
#' str_view(nbsp)
#'
#' # You can also use escapes to see all non-ASCII characters
#' str_view(nbsp, use_escapes = TRUE)
#'
#' # Supply a pattern to see where it matches
#' str_view(c("abc", "def", "fghi"), "[aeiou]")
#' str_view(c("abc", "def", "fghi"), "^")
#' str_view(c("abc", "def", "fghi"), "..")
#'
#' # By default, only matching strings will be shown
#' str_view(c("abc", "def", "fghi"), "e")
#' # but you can show all:
#' str_view(c("abc", "def", "fghi"), "e", match = NA)
#' # or just those that don't match:
#' str_view(c("abc", "def", "fghi"), "e", match = FALSE)
str_view <- function(string, pattern = NULL, match = TRUE, html = FALSE, use_escapes = FALSE) {
  rec <- vctrs::vec_recycle_common(string = string, pattern = pattern)
  string <- rec$string
  pattern <- rec$pattern

  check_bool(match, allow_na = TRUE)
  check_bool(html)
  check_bool(use_escapes)

  filter <- str_view_filter(string, pattern, match)
  out <- string[filter]
  pattern <- pattern[filter]

  if (!is.null(pattern)) {
    out <- str_replace_all(out, pattern, str_view_highlighter(html))
  }
  if (use_escapes) {
    out <- stri_escape_unicode(out)
    out <- str_replace_all(out, fixed("\\u001b"), "\u001b")
  } else {
    out <- str_view_special(out, html = html)
  }

  str_view_print(out, filter, html = html)
}

#' @rdname str_view
#' @usage NULL
#' @export
str_view_all <- function(string, pattern = NULL, match = NA, html = FALSE, use_escapes = FALSE) {
  lifecycle::deprecate_warn("1.5.0", "str_view_all()", "str_view()")

  str_view(
    string = string,
    pattern = pattern,
    match = match,
    html = html,
    use_escapes = use_escapes
  )
}

str_view_filter <- function(x, pattern, match) {
  if (is.null(pattern) || inherits(pattern, "stringr_boundary")) {
    rep(TRUE, length(x))
  } else {
    if (identical(match, TRUE)) {
      str_detect(x, pattern) & !is.na(x)
    } else if (identical(match, FALSE)) {
      !str_detect(x, pattern) | is.na(x)
    } else {
      rep(TRUE, length(x))
    }
  }
}

# Helpers -----------------------------------------------------------------

str_view_highlighter <- function(html = TRUE) {
  if (html) {
    function(x) str_c("<span class='match'>", x, "</span>")
  } else {
    function(x) {
      out <- cli::col_cyan("<", x, ">")

      # Ensure styling is starts and ends within each line
      out <- cli::ansi_strsplit(out, "\n", fixed = TRUE)
      out <- map_chr(out, str_flatten, "\n")

      out
    }
  }
}

str_view_special <- function(x, html = TRUE) {
  if (html) {
    replace <- function(x) str_c("<span class='special'>", x, "</span>")
  } else {
    replace <- function(x) {
      if (length(x) == 0) {
        return(character())
      }

      cli::col_cyan("{", stri_escape_unicode(x), "}")
    }
  }

  # Highlight any non-standard whitespace characters
  str_replace_all(x, "[\\p{Whitespace}-- \n]+", replace)
}

stringr_view <- new_class(
  "stringr_view",
  package = "stringr",
  properties = list(
    x = class_character,
    id = class_numeric
  )
)

str_view_print <- function(x, filter, html = TRUE) {
  if (html) {
    str_view_widget(x)
  } else {
    stringr_view(x, which(filter))
  }
}

str_view_widget <- function(lines) {
  check_installed(c("htmltools", "htmlwidgets"))

  lines <- str_replace_na(lines)
  bullets <- str_c(
    "<ul>\n",
    str_c("  <li><pre>", lines, "</pre></li>", collapse = "\n"),
    "\n</ul>"
  )

  html <- htmltools::HTML(bullets)
  size <- htmlwidgets::sizingPolicy(
    knitr.figure = FALSE,
    defaultHeight = pmin(10 * length(lines), 300),
    knitr.defaultHeight = "100%"
  )

  htmlwidgets::createWidget(
    "str_view",
    list(html = html),
    sizingPolicy = size,
    package = "stringr"
  )
}

base_length <- new_external_generic("base", "length", "x")
method(base_length, stringr_view) <- function(x) {
  length(x@x)
}

base_print <- new_external_generic("base", "print", "x")
method(base_print, stringr_view) <- function(x, ..., n = getOption("stringr.view_n", 20)) {
  n_extra <- length(x) - n
  if (n_extra > 0) {
    x <- x[seq_len(n)]
  }
  id <- x@id
  x <- x@x # extracting the character vector `x` from the S7 object `x` to use in the rest of the function.

  if (length(x) == 0) {
    cli::cli_inform(c(x = "Empty `string` provided.\n"))
    return(invisible(x))
  }

  bar <- if (cli::is_utf8_output()) "\u2502" else "|"

  id <- format(paste0("[", id, "] "), justify = "right")
  indent <- paste0(cli::col_grey(id, bar), " ")
  exdent <- paste0(strrep(" ", nchar(id[[1]])), cli::col_grey(bar), " ")

  x[is.na(x)] <- cli::col_red("NA")
  x <- paste0(indent, x)
  x <- str_replace_all(x, "\n", paste0("\n", exdent))

  cat(x, sep = "\n")
  if (n_extra > 0) {
    cat("... and ", n_extra, " more\n", sep = "")
  }

  invisible(x)
}

`base_[` <- new_external_generic("base", "[", "x")
#' @export
method(`base_[`, stringr_view) <- function(x, i, ...) {
  stringr_view(x@x[i], x@id[i])
}
