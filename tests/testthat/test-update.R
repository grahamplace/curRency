library(curRency)

context("")

test_that("updating values -> data.frame",
          { expect_equal(update("spending_spreadsheet.csv", "USD", 50, "Food", "2016-07-02"), spending_chart) })
