test_that("multiplication works", {

  f <- sample_fringe("Cat-Dat-Num-Pct", n = 11,
                     names = c("Category", "Dates", "Numbers","Percentages"))
  stats <- f$stats

  stats <- fringe_stats(f)$col_stats
  dic_stats <- fringe_dic(f, stats = TRUE)

  expect_equal(names(stats), dic_stats$id)
  expect_equal(unname(stats), dic_stats$stats)

})


