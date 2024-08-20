#' Remove matched patterns
#'
#' Remove matches, i.e. replace them with `""`.
#'
#' @inheritParams str_detect
#' @return A character vector the same length as `string`/`pattern`.
#' @seealso [str_replace()] for the underlying implementation.
#' @export
#' @examples
#' fruits <- c("one apple", "two pears", "three bananas")
#' str_remove(fruits, "[aeiou]")
#' str_remove_all(fruits, "[aeiou]")
str_remove <- function(string, pattern) {
  str_replace(string, pattern, "")
}

#' @export
#' @rdname str_remove
str_remove_all <- function(string, pattern) {
  str_replace_all(string, pattern, "")
}


#' Remove common leading indentation from strings
#' 
#' This function is similar to Python's `dedent` function in the `textwrap` 
#' library. It removes common leading indentation from strings.
#' 
#' @param text `character` The input string or character vector.
#' @return The input string or character vector with leading indentation removed.
#' @export
#' @examples
#' str_dedent("  Hello\n    World")
#' 
#' str_dedent("  Line 1\n  Line 2\n  Line 3")
#' 
#' str_dedent("No indentation")
#' 
#' str_dedent(
#'     "
#'     this
#'     is
#'         a
#'     test
#'     "
#' )
str_dedent <- function(text) {
  lines <- str_split_1(text, fixed("\n"))
  
  # Determine the common leading whitespace
  leading_ws <- ""
  for (line in lines) {
    # Ignore completely blank lines
    if (str_detect(line, "^\\s*$")) {
      next
    }

    ws <- str_extract(line, "^\\s+")
    if (!is.na(ws)) {
      leading_ws <- ws
      break
    }
  }
  
  if (is.null(leading_ws)) {
    return(text)
  }
  
  # Remove the common leading whitespace from each line
  dedented_lines <- str_replace_all(lines, paste0("^", leading_ws), "")
  
  # Combine the lines back into a single string
  paste(dedented_lines, collapse = "\n")
}
