# Small df for reference to the correct format
library(readr)
master_exchange <- read_csv("exchange_master.csv")
Currency <- c("USD", "CuC", "zar")
Value <- c(13, 166, 4)
Category <- c("Clothes", "Travel", "Food")
Date <- as.Date(c("2016-08-25", "2016-10-25", "2016-01-05"))
Template <- data.frame(Currency, Value, Category, Date)
accepted_currencies <- master_exchange[,1]

#' This helper function is doing all the necessary error checking to ensure that the data frame can be correctly processed.
#'
#' authors Carlos Couce, Graham Place
#' @param spending_frame - data frame passed through input() by user before convert() is called
#' @param out_curr - all the currencies in the data frame will
#'                          be converted to this output currency
#' @return spending_frame - generate a column at the far right of the data frame
#'               with the desired output_currency and a running total
error_checking <- function(spending_frame, out_curr) {
  if (class(spending_frame) != "data.frame") {
    stop("Input a data frame that was passed through input()!")
  }
#  if (!all(colnames(Template) == colnames(spending_frame))) {
 #   stop("Use the template provided!")
 # }
 # if (class(out_curr) != "character") {

  #  stop("Second argument must be a string.")
  #}
 # if (!is.element(toupper(out_curr), accepted_currencies)) {
  #  stop("Second argument is not a currency!")
  #}
  #for (currency in spending_frame$Currency) {
   # if (!is.element(toupper(currency), master_exchange$Code)) {
    #  cat(sprintf("%s is not in our database. Check the spelling.\n", currency))
    #}
 # }
}

#' This function will convert between currencies.
#' authors Carlos Couce, Graham Place
#' @param spending_frame - data frame passed through input() by user before convert() is called
#' @param out_curr - all the currencies in the data frame will
#'                          be converted to this output currency
#' @return spending_frame - generate a column at the far right of the data frame
#'               with the desired output_currency and a running total
#' @export
convert <- function(spending_frame, out_curr = "USD") {
  library(readr)

  load("data/exchange_master.rda")
  master_exchange <- exchange_master

#  master_exchange <- read_csv("exchange_master.csv")
 # error_checking(spending_frame, out_curr)
  counter <- 0
  spending_frame[,"Output"] <- NA
  for (curr in spending_frame$Currency) {
    counter <- counter + 1
    to.US <- master_exchange[match(toupper(curr), master_exchange$Code), 2]
    middle <- to.US * spending_frame[counter, 2]
    to.desired <- master_exchange[match(toupper(out_curr), master_exchange$Code), 3]
    final <- middle * to.desired
    spending_frame[counter, 5] <- final
  }
  write.table(spending_frame, paste(paste("Converted_Spending_", toupper(out_curr), sep = ""), ".csv", sep = ""))
  return(spending_frame)
}
