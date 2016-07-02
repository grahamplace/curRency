#' Read in spreadsheet and added value, convert and add to data frame
#'
#' @param csv A csv file with expenses information (including currency).
#' @param currency The currency of the added value.
#' @param value The new monetary value.
#' @param category The category of expense.
#' @param date The date of expense.
#' @return A data frame with csv file values and update value information
#' @example
#' update("spending_spreadsheet.csv", "ZAR", 50, "Food", 2016-07-02)
#' @export

update <- function(csv, currency, value, category, date) {
  library(readr)
  input(csv)
  update_val <- data.frame("Currency" = currency, "Value" = value, "Category" = category, "Date" = date)
  spending_chart <- rbind(spending_chart, update_val)
  return(spending_chart)
}
