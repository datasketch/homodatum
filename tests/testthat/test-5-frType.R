test_that("frType", {

  frtype <- frType("Num-Cat")

  expect_true(inherits(c(frType("Num"), "Cat"),"frType"))

  y <- frType("Cat")
  expect_true(frType("Cat") == y)




})
