test_that("Yea", {

  # Yea
  x <- new_Yea(c(1803:1800, NA))
  x
  str(x)
  expect_true(Yea_get_stats(x)$is_sequence)



  expect_equal(Yea_get_stats(x)$n_na, 1)
  expect_equivalent(vctrs::vec_cast(x, integer()), c(1803:1800, NA))

  # data <- data.frame(
  #   e = Yea(2000:2003)
  # )
  # write.csv(data,"~/Downloads/test.csv")


})
