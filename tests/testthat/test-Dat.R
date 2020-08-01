test_that("Dat", {

  x <- c("06/24/2003")
  d <- new_Dat(x)
  d
  expect_equal(Dat_get_format(d), "%m/%d/%Y")
  expect_equal(Dat_get_order(d), "mdy")
  expect_equal(Dat_show(d), x)
  expect_equal("06/2003",Dat_show(d, format = "%m/%Y"))

  x <- c("7.12.2000")
  d <- new_Dat(x)
  d
  expect_equal(Dat_get_format(d), "%d.%m.%Y")
  expect_equal(Dat_get_order(d), "dmy")
  expect_equal("2000.12",Dat_show(d, format = "%Y.%m"))

  x <-   c("2000-12-04","2010-20-04")
  dat <- new_Dat(x, format = "%Y-%d-%m")
  dat

  x <- c("2000-12-31", "2100-11-30", NA)
  dat <- new_Dat(x)
  dat

  #Dat(NULL)
  x <- Dat("2000-12-04")
  y <- new_Dat("2000-04-12", format = "%Y-%d-%m")
  expect_equal(vctrs::vec_data(x),vctrs::vec_data(y))
  expect_equal(Dat_get_isodate(x),as.character(vctrs::new_date(vctrs::vec_data(y))))

  z <- Dat(as.Date("2000-12-04"))
  expect_equal(x,z)
  w <- Dat(c(as.Date("2000-12-04"),NA))
  expect_true(is.na(w[2]))


  expect_true(is.na(Dat(NA)))
  # new_Dat(c(NA,NA))
  # Dat(c(NA,NA))
  expect_equal(is.na(Dat(c(NA,NA))), c(TRUE, TRUE))

  z <- Dat(c("2000-12-04","2010-20-04"), format = "%Y-%d-%m")
  # stats <- Dat_get_stats(z)
  # stats$min
  # Dat_get_isodate(z)
  # expect_equal(unlist(stats))

  # Works with POXIXct
  x <- as.POSIXct("2020-04-30")
  class(x)
  expect_equal(Dat(x), Dat("2020-04-30"))

  # TODO stats need to return Dat as well? Define min and max for Dat

  # Accepts anything coercible from double()
  x <- Dat(c("1","0.2"))
  class(x)
  expect_true(inherits(x, "hd_Dat"))

  # x <- Dat(c(1,1,2,2,3,3))
  # x

  # TODO TEST CASTS FROM DATES, NUMBERS, ETC
  dates <- seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 3)
  Dat(dates)

  ## Dat("21\04?2020") # CHECK POSSIBLE PROBLE WITH SCAPING

  string <- "21|04?2020"
  dat <- Dat(string)

  expect_equal(as.character(dat), string)
  expect_equal(as.Date(dat), as.Date("2020-04-21"))
  expect_equal(as.numeric(dat), vctrs::vec_data(dat))


  # TODO IS THIS COERCION NECESSARY?
  # c(Dat("2020-04-30"),"2020-04-30")


  a <- data.frame(fechas = Dat(c("2020-04-10", "2020-04-20")))
  tibble::tibble(a)
})
