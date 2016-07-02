# Small df for reference to the correct format
Currency <- c("USD", "CuC", "zar")
Value <- c(13, 166, 4)
Category <- c("Clothes", "Travel", "Food")
Date <- as.Date(c("2016-08-25", "2016-10-25", "2016-01-05"))
Template <- data.frame(Currency, Value, Category, Date)
accepted_currencies <- exchange_master[,1]


# This helper function is doing all the necessary error checking to
# ensure that the data frame can be correctly processed.
error_checking <- function(csv, out_curr) {
  if (class(csv) != "data.frame") {
    stop("Input a csv file!")
  }
  if (!all(colnames(Template) == colnames(csv))) {
    stop("Use the template provided!")
  }
  if (class(out_curr) != "character") {
    stop("Second argument must be a string.")
  }
  if (!is.element(toupper(out_curr), accepted_currencies)) {
    stop("Second argument is not a currency!")
  }
  for (currency in csv$Currency) {
    if (!is.element(toupper(currency), exchange_master$Code)) {
      cat(sprintf("%s is not in our database. Check the spelling.\n", currency))
    }
  }
}

#' This function will convert between currencies.
#' @author Carlos Couce
#' @param csv - file with spending expenses
#' @param out_curr - all the currencies in the csv will
#'                          be converted to this output currency
#' @return csv - generate a column at the far right of the csv
#'               with the desired output_currency and a running total
#' @example convert("my-summer-expenses", "USD")
#' @export
convert <- function(csv, out_curr) {
  error_checking(csv, out_curr)
  counter <- 0
  csv[,"Output"] <- NA
  for (curr in csv$Currency) {
    counter <- counter + 1
    to.US <- exchange_master[match(toupper(curr), exchange_master$Code), 2]
    middle <- to.US * csv[counter, 2]
    to.desired <- exchange_master[match(toupper(out_curr), exchange_master$Code), 3]
    final <- middle * to.desired
    csv[counter, 5] <- final
  }
  write.table(csv, "New_Ouput")
}
