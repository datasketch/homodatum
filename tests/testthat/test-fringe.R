test_that("fringe", {

  f <- new_fringe(cars)
  length(f) # 4 ----> Expect 1
  fr <- fringe(cars)
  expect_equal(f$data, fr$data)
  expect_equal(f$dic, fr$dic)
  # expect_equal(f$frtype, fr$frtype) ### TODO
  expect_equal(f$group, fr$group)

  f2 <- list(fringe(mtcars), fringe(cars))
  expect_true(inherits(fringe(cars),"fringe"))

  hd <- sampleData("Cat-Cat-Num-Pct")
  fringe(hd)

  fr <- fringe(cars, name = "Los Carros", mas = "fda")
  expect_true(fr$meta$mas == "fda")
  expect_true(fr$slug == "los-carros")

  dir <- tempdir(check = TRUE)
  # dir <- "~/Downloads/tmp"
  ### for some reason this tempdir clashes with write_csv(data, "")
  write_fringe(fr, path = dir, overwrite_dic = TRUE)
  expect_true(all(file.exists(file.path(dir,
                                        c('los-carros.csv',
                                          'los-carros.dic.csv',
                                          'los-carros.yaml')))))
  unlink(dir, recursive = TRUE)

})
