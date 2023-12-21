#' Check `character` arguments
#'
#' @description 
#' Checks:
#'   - Provided
#'   - Not null
#'   - Not empty
#'   - Not NA
#'   - Of type `character`
#'   - Of `length > 0`
#' 
#' @noRd

is_character <- function(str) {
  
  if (missing(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", 
         call. = FALSE)
  }
  
  if (is.null(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.character(str)) {
    stop("Argument '", deparse(substitute(str)), "' must be a character", 
         call. = FALSE)
  }
  
  if (length(str) == 0) {
    stop("Argument '", deparse(substitute(str)), "' must be of length > 0", 
         call. = FALSE)
  }
  
  if (any(is.na(str))) {
    stop("Argument '", deparse(substitute(str)), "' cannot contain NA", 
         call. = FALSE)
  }
  
  invisible(NULL)
}
