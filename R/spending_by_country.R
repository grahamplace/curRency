#' Display bar chart broken down by spending per country in desired output currency
#' author Graham Place
#'
#' @param spending_frame A data frame with expenses information (including currency)
#' @param outputCurr desired output currency to display totals in
#' @export
spending_by_country <- function(spending_frame, outputCurr = "USD") {
  library(ggplot2)
  library(dplyr)
  spending_frame <- convert(spending_frame, outputCurr)
  spending_frame$Currency <- toupper(spending_frame$Currency)
  withTotals<- aggregate(Output ~ Currency, spending_frame, sum)
  withTotals$Rank <- rank(withTotals$Output)

  #Plot:
  ggplot(withTotals, mapping = aes(x = Currency, y = Output, fill = Rank^2)) +
    geom_bar(stat = "identity") +
    labs(x = "Currency", y = paste("Total Spending in", toupper(outputCurr)), title = paste("Total Spending By Country in", toupper(outputCurr))) +
    guides(fill = FALSE)
}
