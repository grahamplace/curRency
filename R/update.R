#' Read in spreadsheet and added value, convert and add to data frame
#' @authors Claire Adair, Graham Place
#'
#' @param csv A data frame already passed through input() function.
#' @param currency String containing currency code of the added value.
#' @param value Integer of expense in original purchase's currency
#' @param category String containing the category of expense.
#' @param date String containing date of expense in YYYY-MM-DD format.
#' @return A data frame with user inputted data appended at bottom
#' @example
#' update(my_spending, "ZAR", 50, "Food", "2016-07-02")
#' @export
update <- function(csv, currency, value, category, date) {
  library(readr)
  spending_chart <- input(csv)
  update_val <- data.frame("Currency" = currency, "Value" = value, "Category" = category, "Date" = date)
  spending_chart <- rbind(spending_chart, update_val)
  write.csv(spending_chart, "updated_spending_chart.csv")
  return(spending_chart)
}

