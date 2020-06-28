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

test_that("Fringe creation with dictionaries work",{

  d <- cars
  names(d) <- c("a", "b")
  dic <- data.frame(label = c("speed", "dist"),
                    id = c("speed", "dist"),
                    hdType = c("Num", "Num"))
  f <- fringe(d, dic = dic)
  expect_equivalent(fringe_d(f), d)
  dic <- data.frame(label = c("Speed", "Dist"),
                    id = c("speed", "dist"),
                    hdType = c("Num", "Cat"))
  f2 <- fringe(d, dic = dic)
  expect_equivalent(fringe_d(f2)[[2]], as.character(cars[[2]]))

  dic <- data.frame(label = c("speed", "dist"),
                    id = c("speed", "dist"))
  f3 <- fringe(d, dic = dic)
  expect_equal(f3, f)

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
  expect_equivalent(hdType(dic$hdType), f$dic$hdType)

  f_data <- fringe_data(f)
  expect_false("hd_tbl" %in% class(f_data))

  f_hdTibble <- fringe_hdTibble(f)
  expect_true("hd_tbl" %in% class(f_hdTibble))

  #

  data <- data.frame(
    a = Cat(c("black", "white")),
    b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
    c = Yea(2001:2002),
    d = Num(runif(2)*10),
    e = Pct(runif(2))
  )
  class(data)
  f <- fringe(data)
  f_data <- fringe_data(f)

  expect_false("hd_tbl" %in% class(f_data))

  f_hdTibble <- fringe_hdTibble(f)
  expect_true("hd_tbl" %in% class(f_hdTibble))

})

