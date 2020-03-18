test_that("Pct is well defined", {

  # Pct
  x <- new_Pct(c(seq(0, 1, length.out = 4), NA))
  x
  str(x)

  #Pct(NULL)
  Pct(NA)

  # Accepts anything coercible from double()
  x <- Pct(c("1","0.2"))
  x
  y <- Pct(c("100%","20%")) # If there is at least 1 %
  y
  expect_equal(x,y)

  expect_error(Pct(c("100%","20"))) # All must have the % symbol
  expect_error(Pct(c("%100"))) # All must have the % symbol at the end
  expect_error(Pct("#4"))

  a <- data.frame(percent = Pct(c(1:10)/100))
  tibble(a)


  # Casts: Pct to Nu
  # check implementation of coercion
  vec_ptype_show(Pct(), double(), Pct())

  vec_cast(0.5, Pct())
  vec_cast(Pct(0.5), double())

  vec_c(Pct(0.5), 1)
  vec_c(NA, Pct(0.5), 1)
  # vec_c(TRUE, Pct(0.5), 1)
})
