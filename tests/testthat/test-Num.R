test_that("Num",{
  # Num
  x <- new_Num(c(seq(0, 1, length.out = 4), NA))
  x
  str(x)
  expect_equal(Num_get_stats(x)$min, 0)
  expect_equal(Num_get_stats(x)$max, 1)
  expect_equivalent(vctrs::vec_cast(x, double()), c(seq(0, 1, length.out = 4), NA))

  c(Num(1),1)
  c("Num", hdType("Num"))

  vctrs::vec_ptype2(1, Num())
  vctrs::vec_ptype2(Num(),1)

  vctrs::vec_ptype_show(Num(), double(), Num())

  vctrs::vec_cast(1, Num())
  vctrs::vec_cast(Num(1), double())

  vctrs::vec_cast(Num(1), character())
  vctrs::vec_cast("1", Num())

  data <- data.frame(
    e = Num(runif(2))
  )
  write.csv(data,"~/Downloads/test.csv")

})
