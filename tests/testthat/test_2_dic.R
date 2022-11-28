test_that("dic",{

  d <- mtcars
  new_dic <- create_dic(d)
  expect_equal(new_dic$hdType, frType_hdTypes(guess_frType(d)))

  d <- cars
  new_dic <- create_dic(d, frtype = "Num-Num")
  expect_equal(new_dic$hdType, hdType(c("Num", "Num")))

  d <- sample_data("Cat-Num")
  new_dic <- create_dic(d)
  expect_equal(firstup(names(d)), new_dic$label)

  d <- tibble::tibble(x = Cat(1:2), y = Num(1:2))
  new_dic <- create_dic(d)
  expect_equal(new_dic$hdType, hdType(c("Cat", "Num")))

  d <- tibble::tibble(x = "24/06/2020")
  new_dic <- create_dic(d)
  expect_equal(new_dic$hdType, hdType("Dat"))


})
