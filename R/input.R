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
  names(user_spending)[1] <- 'Currency'
  names(user_spending)[2] <- 'Value'
  names(user_spending)[3] <- 'Category'
  names(user_spending)[4] <- 'Date'
 # spending_chart <- data.frame(Currency = user_spending[,1], Value = user_spending[,2], Category = user_spending[,3], Date = user_spending[,4])
  user_spending$Category <- factor(user_spending$Category)
  return(user_spending)
}

names(user_spending)[1] <- 'Currency'
names(user_spending)[2] <- 'Value'
names(user_spending)[3] <- 'Category'
names(user_spending)[4] <- 'Date'
