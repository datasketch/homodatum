test_that("Select columns", {

  f <- sample_fringe("Cat-Dat-Num-Pct", n = 11,
                     names = c("Category", "Dates", "Numbers","Percentages"))
  f$stats

  columns <-  c("Dates", "Numbers")


  f2 <- select_columns(f, columns)

  columns <-  c("Category", "Numbers")
  f2 <- select_columns(f, columns)

  stats <- homodatum::fringe_dic(f2, stats = TRUE)

  x <- hdType(c("Category1"="Cat","Numeric"="Num", "Category2" = "Cat"))
  hdtypes_subset(x, frtype = "Cat-Num")
  hdtypes_subset(x, frtype = "Cat")

  f3 <- fringe_subset_columns(f, frtype = "Cat-Num")
  expect_equal(fringe_frtype(f3), "Cat-Num")

  # sample_data_path <- sys_file("sample_data", "nombramientos_ecuador.csv")
  # d <- read.csv(sample_data_path)
  # f <- fringe(d)
  # #dic <- fringe_dic(f, stats = TRUE)
  # suggest_columns(f, frtype = "Cat-Num")


  f <- sample_fringe("Cat-Dat")
  f2 <- suggest_columns(f, frtype = "Cat-Num")

})
