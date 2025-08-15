#' Validate Date String
#'
#' Validates if a date string in YYYYMMDD format represents a valid date
#'
#' @param date_string Character vector of date strings in YYYYMMDD format
#' @return Logical vector indicating which dates are valid
#' @export
#'
#' @examples
#' .validateDateString("20250814")  # TRUE
#' .validateDateString("20250230")  # FALSE (Feb 30 doesn't exist)
#' .validateDateString("20251301")  # FALSE (month 13 doesn't exist)
.validateDateString <- function(date_string) {
  if (!is.character(date_string)) {
    stop("Input must be a character vector")
  }
  
  # Check if string has exactly 8 digits
  if (!all(nchar(date_string) == 8 & grepl("^\\d{8}$", date_string))) {
    return(rep(FALSE, length(date_string)))
  }
  
  result <- logical(length(date_string))
  
  
  tryCatch({
    parsed_date <- as.Date(date_string, format = "%Y%m%d")
    # Check if the parsed date when formatted back matches the original
    formatted_back <- format(parsed_date, "%Y%m%d")
    result <- !is.na(parsed_date) && formatted_back == date_string
  }, error = function(e) {
    result[i] <- FALSE
  })
  
  
  return(result)
}

#' Validate String Pattern
#'
#' Validates if a string matches the expected pattern with valid date:
#' YYYYMMDD_###_C#####_S######_UK##_##_##
#'
#' @param x Character vector to validate
#' @return Logical vector indicating which strings match the pattern
#' @export
#'
#' @examples
#' .validateFilename("20250814_005_C39408_S990965_UK15_01_26")  # TRUE
#' .validateFilename("20250230_005_C39408_S990965_UK15_01_26")  # FALSE (invalid date)
#' .validateFilename("invalid_string")  # FALSE
.validateFilename <- function(x) {
  stopifnot(is.character(x))
  
  #pattern <- "^\\d{8}_\\d{3}_C\\d{5}_S\\d{6}[-a-zA-Z0-9_]+$"
  pattern <- "^\\d{8}_\\d{3}_[-a-zA-Z0-9_]+$"
  
  # Check basic pattern first
  matches_pattern <- grepl(pattern, x)
  
  if (isFALSE(matches_pattern)){
    return(FALSE)
  }
  
  dateStringPart <- substr(x, 1, 8)
  
  if (isFALSE(.validateDateString (dateStringPart))){
    return(FALSE)
  }
  
  return(TRUE)
}