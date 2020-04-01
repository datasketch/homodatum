test_that("fringe", {

  f <- new_fringe(cars)
  length(f)
  fringe(cars)

  f2 <- list(fringe(mtcars), fringe(cars))

  class(f2[[1]])

  expect_true(inherits(fringe(cars),"fringe"))


  fringe(cars, mas = "fda")
  fringe(iris)

  f1 <- fringe(cars)

  f1$data
  f1$name
  vctrs::vec_data(f1)

  write_fringe(f1, path = "~/Downloads", overwrite_dic = TRUE)


})
