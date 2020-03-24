context("dic")

test_that("dic",{

  d <- mtcars
  dd <- make_dic(d)
  expect_equal(dd$dic$hdType, frType_hdTypes(guess_frType(d)))

  d <- cars
  dd <- make_dic(d, frtype = "Num-Num")
  expect_equal(dd$dic$hdType, hdType(c("Num", "Num")))



})
