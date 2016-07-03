#' Display bar chart broken down by day of the week
#'
#' @param spending_frame A data frame with expenses information (including currency)
#' @param outputCurr desired output currency to display totals in
#' @example
#' spending_by_day(my_spending, "ZAR")
#' @export
spending_by_day <- function(spending_frame, outputCurr = "USD") {
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  spending_frame <- convert(spending_frame, outputCurr)
  #spending_frame$Date <- ymd(spending_frame$Date)
  spending_frame$Day <- weekdays(spending_frame$Date)
  spending_frame$Day <- as.factor(spending_frame$Day)
  withTotals<- aggregate(Output ~ Day, spending_frame, sum)
  set <- data.frame(Day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  set$Total_Spending <- 0
  set$Total_Spending <- withTotals[match(set$Day, withTotals$Day), 2]
  set$Total_Spending[which(is.na(set$Total_Spending))] <- 0
  set$Rank <- rank(set$Total_Spending)

  #Plot:
  ggplot(set, mapping = aes(x = Day, y = Total_Spending, fill = Rank^2)) +
    scale_x_discrete(limits =  c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
    geom_bar(stat = "identity") +
    labs(x = "Weekday", y = paste("Total Spending in", toupper(outputCurr)), title = paste("Total Spending By Category in", toupper(outputCurr))) +
    guides(fill = FALSE)
}
