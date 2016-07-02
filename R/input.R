#' Read in spender's spreadsheet and convert to usable data frame
#'
#' @param csv A csv file with expenses information (including currency).
#' @return A data frame with cleaned up and properly identified information.
#' @example
#' input("spending_spreadsheet.csv")
#' @export

input <- function(csv) {
  if (class(csv) != "character") {
    stop("csv file must be input as a string!")
  }
  if (csv == "\\b[:alnum:]*[:punct:]*[^.csv]\\b") {
    stop("inputted file must be a csv!")
  }
  library(readr)
  user_spending <- read_csv(csv)
  spending_chart <- data.frame("Currency" = user_spending[,1], "Value" = user_spending[,2], "Category" = user_spending[,3], "Date" = user_spending[,4])
  spending_chart$Category <- factor(spending_chart$Category)
  return(spending_chart)
}
