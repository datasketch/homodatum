test_that("Num",{
  # Num
  x <- new_Num(c(seq(0, 1, length.out = 4), NA))
  x
  str(x)
  expect_equal(Num_get_stats(x)$min, 0)
  expect_equal(Num_get_stats(x)$max, 1)
  expect_equivalent(vec_cast(x, double()), c(seq(0, 1, length.out = 4), NA))




})
