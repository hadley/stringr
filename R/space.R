#' Split up strings
#'
#' @description
#' `str_space()` adds a space wherever a lowercase letter is followed by an 
#' uppercase letter, with modifiable functionality for special cases.
#'
#' @param split_style controls the placement of splits and can be 
#' modified to the following styles:
#' `acronyms`: splits at lowercase-uppercase and between two uppercase letters 
#' followed by lowercase,
#' `allcaps`: splits between all consecutive uppercase letters
#' `default`: splits only when a lowercase letter is succeeded by an uppercase letter
#' `everything`: splits between every character
#' 
#' @param split_char controls the character that splits words.
#' This defaults to space but can be changed to anything.
#'
#' @seealso [str_indent()] to add tabs between words.
#' @export
#' @examples
#' input <- "XMLHTTPRequestAndHTMLParser"
#' # Default splitting
#' str_space(input, split_style = "default")
#' # Split acronyms
#' str_space(input, split_style = "acronyms")
#' # Split all consecutive uppercase letters
#' str_space(input, split_style = "allcaps")
#' # Split every character
#' str_space(input, split_style = "everything")
#'

str_space <- function(x, split_style = "default", split_char = " ") {
  if (split_style == "acronyms") {
    # Splits at lowercase-uppercase and between two uppercase letters followed by lowercase
    str_replace_all(x, "(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])", split_char)
  } else if (split_style == "allcaps") {
    # Splits between all consecutive uppercase letters
    str_replace_all(x, "(?<=\\p{Lu})(?=\\p{Lu})", split_char)
  } else if (split_style == "default") {
    # Splits only when a lowercase letter is succeeded by an uppercase letter
    str_replace_all(x, "(?<=[a-z])(?=[A-Z])", split_char)
  } else if (split_style == "everything") {
    # Splits between every character
    str_replace_all(x, "(?<=.)(?=.)", split_char)
  } else { 
    stop("Invalid split_style. Choose from 'default', 'acronyms', 'allcaps', or 'everything'.")
  }
}
