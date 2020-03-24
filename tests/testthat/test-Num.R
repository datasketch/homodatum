test_that("Num",{
  # Num
  x <- new_Num(c(seq(0, 1, length.out = 4), NA))
  x
  str(x)
  expect_equal(Num_get_stats(x)$min, 0)
  expect_equal(Num_get_stats(x)$max, 1)
  expect_equivalent(vec_cast(x, double()), c(seq(0, 1, length.out = 4), NA))

  c(Num(1),1)
  c("Num", hdType("Num"))

  vec_ptype2(1, Num())
  vec_ptype2(Num(),1)

  vec_ptype_show(Num(), double(), Num())

  vec_cast(1, Num())
  vec_cast(Num(1), double())

  vec_cast(Num(1), character())
  vec_cast("1", Num())

  data <- data.frame(
    e = Num(runif(2))
  )
  write_csv(data,"~/Downloads/test.csv")

})
