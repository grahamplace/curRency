#' Display bar chart broken down by spending category
#'
#' @param spending_frame A data frame with expenses information (including currency)
#' @param outputCurr desired output currency to display totals in
#' @example
#' spending_bar(my_spending, "ZAR")
#' @export
spending_bar <- function(spending_frame, outputCurr = "USD") {
  library(ggplot2)
  library(dplyr)
  spending_frame <- convert(spending_frame, outputCurr)

  ###
  #Best solution after lots of trial and error:
  withTotals<- aggregate(Output ~ Category, spending_frame, sum)
  withTotals$Rank <- rank(withTotals$Output)

  #Plot:
  ggplot(withTotals, mapping = aes(x = Category, y = Output, fill = Rank^2)) +
    geom_bar(stat = "identity") +
    labs(x = "Spending Category", y = paste("Total Spending in", toupper(outputCurr)), title = paste("Total Spending By Category in", toupper(outputCurr))) +
    guides(fill = FALSE)


  #notes/trials:
  #agg <- group_by(spending_frame, Category)
 # spending_frame %>% dplyr::group_by(Category) %>% summarize(Total_Spending = sum(Output))
  #str(spending_frame)
 # as.data.frame(group_by(spending_frame, Category))
  ##from stack:
  #x %>%
   # group_by(Category) %>%
    #summarise(Frequency = sum(Frequency))
  #aggregate(Frequency ~ Category, x, sum)
  #agg <- group_by(spending_frame, Category)
  #withTotals$Total_Spending <- summarize(agg, Total_Spending = sum(Output))
  #withTotals$Rank <- rank(withTotals$Total_Spending)
}
