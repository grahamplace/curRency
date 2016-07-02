library(to.dataframe)

context("")

test_that("converting csv -> data.frame",
          { expect_equal(to.dataframe("spending_spreadsheet.csv"), user_spending) })
