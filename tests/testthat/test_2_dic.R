context("dic")

test_that("dic",{

  d <- mtcars
  dd <- add_dic(d)
  expect_equal(dd$dic$hdType, frType_hdTypes(guess_frType(d)))

  d <- cars
  dd <- add_dic(d, frtype = "Num-Num")
  expect_equal(dd$dic$hdType, hdType(c("Num", "Num")))

  d <- sampleData("Cat-Num")
  with_dic <- add_dic(d)
  expect_equal(firstup(names(d)), with_dic$dic$label)

  d <- tibble::tibble(x = Cat(1:2), y = Num(1:2))
  d_dic <- add_dic(d)
  expect_equal(d, d_dic$data)
  expect_equal(d_dic$dic$hdType, hdType(c("Cat", "Num")))

})
