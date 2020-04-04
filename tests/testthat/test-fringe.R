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

  d <- sampleData("Cat-Dat-Num-Pct",
                  names = c("Category", "Dates", "Numbers","Percentages"))
  f <- fringe(d)
  names(d) <- make_slug(names(d))
  expect_equal(d, f$data)

  expect_equal(pullFringeColumn(f,1),f$data[[1]])
  expect_equal(pullFringeColumn(f,"Dates"), f$data[[2]])
  expect_equal(pullFringeColumn(f,"c"), getFringeDataFrame(f)[[3]])

  dd <- getFringeDataFrame(f)
  expect_equal(names(dd), c('a','b','c','d'))
  expect_equal(purrr::map_chr(dd, class),
               c(a = "character", b = "Date", c = "numeric", d = "numeric"))
  expect_true(all(!purrr::map_lgl(dd, is_hdType)))

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
