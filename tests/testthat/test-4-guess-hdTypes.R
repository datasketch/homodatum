test_that("Test guess", {

  # Guess Num
  expect_equal(guess_hdType(c("1",NA,"2")), hdType("Num"))
  expect_equal(guess_hdType(c(0.3, 2, NA)), hdType("Num"))

  # Guess Pct
  expect_equal(guess_hdType(c(0.3, 0.4, 1)), hdType("Pct"))
  expect_equal(guess_hdType(c("30%", "200%", NA)), hdType("Pct"))

  # Guess Dat
  v <- c("2020-04-04", NA)
  expect_equal(guess_hdType(v), hdType("Dat"))

  v <- "24/06/2020"
  expect_true(isDate(v))
  expect_equal(guess_hdType(v), hdType("Dat"))

  v <- "24/6/2020"
  expect_true(isDate(v))
  expect_equal(guess_hdType(v), hdType("Dat"))
  h <- Dat(v)
  attributes(h)$format

  d <- tibble::tibble(x = "24/06/2020",
                      y = "2020/12/31",
                      z = "2020:05:20")
  expect_equal(as.character(guess_frType(d)),"Dat-Dat-Dat")
  f <- fringe(d)
  expect_equal(attributes(f$data[[1]])$format, "%d/%m/%Y")
  expect_equal(attributes(f$data[[2]])$format, "%Y/%m/%d")
  expect_equal(attributes(f$data[[3]])$format, "%Y:%m:%d")


  # v <- c("2014-03-01","2043-04-04","2014-04-04")
  # isDate(v)
  # isDatetime(v)
  # isTime(v)
  # whichDTH(v)
  # v <- c("2014-03-01","2043-04-04","20140404")
  # parseDatetime(v, "D")
  # v <- c("2014-03-01 5:04:00","2043-04-04 5:04:00","2014-04-04 5:04:00")
  # parseDatetime(v, "T")
  # v <- c("04:00","13:05:00","5:04:00")
  # parseDatetime(v, "H")

})
