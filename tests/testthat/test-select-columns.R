test_that("Select columns", {

  f <- sample_fringe("Cat-Dat-Num-Pct", n = 11,
                     names = c("Category", "Dates", "Numbers","Percentages"))
  f$stats

  columns <-  c("Dates", "Numbers")


  f2 <- select_columns(f, columns)

  columns <-  c("Category", "Numbers")
  f2 <- select_columns(f, columns)

  homodatum::fringe_dic(f2)


})
