#' Display bar chart broken down by spending category
#' @author Graham Place
#'
#' @param spending_frame A data frame with expenses information (including currency)
#' @param outputCurr desired output currency to display totals in
#' @example
#' spending_line_total(my_spending, "ZAR")
#' @export
spending_line_total <- function(spending_frame, outputCurr = "USD") {
  library(ggplot2)
  library(dplyr)
  spending_frame <- convert(spending_frame, outputCurr)

  withTotals<- aggregate(Output ~ Date, spending_frame, sum)
  withTotals$Rank <- rank(withTotals$Output)

  #Plot:
  ggplot(withTotals, mapping = aes(x = withTotals$Date, y = withTotals$Output)) +
    geom_line(stat = "identity") +
    labs(x = "Date", y = paste("Total Spending in", toupper(outputCurr)), title = paste("Spending Over Time in", toupper(outputCurr))) +
    guides(fill = FALSE)
}
