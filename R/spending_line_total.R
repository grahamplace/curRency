#' Display bar chart broken down by spending category
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
  agg <- group_by(spending_frame, Date)
  withTotals <- summarise(agg, Total_Spending = sum(Output))
  withTotals$Rank <- rank(withTotals$Total_Spending)
  ggplot(withTotals, mapping = aes(x = Category, y = Total_Spending, fill = Rank^2)) +
    geom_bar(stat = "identity") +
    labs(x = "Category", y = "Total Spending", title = "Total Spending By Category") +
    guides(fill = FALSE)
}
