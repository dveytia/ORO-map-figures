#' Clean strings
#'
#' @description 
#' Cleans a vector of strings by removing multiple whitespace, number, 
#' punctuation, etc.
#' 
#' @param x a vector of characters. Typically a vector of titles/abstracts to 
#'   clean.
#' 
#' @export

clean_string <- function(x) {
  
  ## Check args ----
  
  is_character(x)
  
  
  ## Clean ----
  
  x <- gsub("\n", " ", x)          # Remove new lines
  x <- gsub("- ", "-", x)          # Deal with caesura
  x <- gsub("[[:punct:]]", " ", x) # Remove punctuation
  x <- gsub("[0-9]", "", x)        # Remove numbers
  x <- gsub("\\s+", " ", x)        # Remove whitespaces
  x <- gsub("^\\s|\\s$", "", x)    # Remove whitespaces
  
  x <- tolower(x)
  x 
}
