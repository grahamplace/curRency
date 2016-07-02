#' Read in spender's spreadsheet and convert to usable data frame
#'
#' @param csv A csv file with expenses information (including currency).
#' @return A data frame with cleaned up and properly identified information.
#' examples
#' to.dataframe("spending_spreadsheet")
#' @export

to.dataframe <- function(csv) {
  if (class(csv) != "character") {
    stop("csv file must be input as a string!")
  }
  if (csv == "\\b[:alnum:]*[:punct:]*[^.csv]\\b") {
    stop("inputted file must be a csv!")
  }
  library(readr)
  user_spending <- read_csv(csv)
  user_spending[,3] <- factor(user_spending[,3])
}
to.dataframe("spending_spreadsheet")
