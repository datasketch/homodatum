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


  d <- sample_data("Cat-Dat-Num-Pct", n = 11,
                  names = c("Category", "Dates", "Numbers","Percentages"))
  f <- fringe(d)
  names(d) <- make_slug(names(d))
  expect_equal(d, f$data)

  expect_equal(fringe_stats(f)$nrow, 11)
  expect_equal(fringe_stats(f)$ncol, 4)


})

test_that("frige column extraction works",{

  f <- sample_fringe("Cat-Dat-Num-Pct", n = 11,
                     names = c("Category", "Dates", "Numbers","Percentages"))
  f
  expect_equal(fringe_column(f,1),as_baseType(f$data[[1]]))
  expect_equal(fringe_column(f,"Dates"), as_baseType(f$data[[2]]))
  expect_equal(fringe_column(f,"c"), fringe_d(f)[[3]])

})



test_that("frige column extraction works",{

  f <- sample_fringe("Cat-Dat-Num-Pct", n = 11,
                     names = c("Category", "Dates", "Numbers","Percentages"))
  dd <- fringe_d(f)
  expect_equal(names(dd), c('a','b','c','d'))
  expect_equal(purrr::map_chr(dd, class),
               c(a = "character", b = "Date", c = "numeric", d = "numeric"))
  expect_true(all(!purrr::map_lgl(dd, is_hdType)))


  # New fringe with dic

  data <- data.frame(book = c("Black", "Red"),
                     value = 1:2,
                     dates = c("28/04/2019", "4/12/2018"))
  dic <- data.frame(id = names(data),
                    hdType = c("Cat","Num","Dat"))
  f <- fringe(data, dic = dic)

  expect_equal(dic$id, f$dic$id)
  f$dic
  expect_equivalent(hdType(dic$hdType), f$dic$hdType)


  f <- fringe(data)

  data <- fringe_data(f)
  dic <- fringe_dic(f)

  dic2 <- dic
  dic$hdType[3] <- "Dat"


})

