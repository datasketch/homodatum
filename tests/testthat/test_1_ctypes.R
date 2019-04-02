context("ctypes")

test_that("Guess Ctypes",{

  void = data_frame(col1 = character(0), col2 = character(0))
  expect_equal(guessCtype(void[1]),"___")
  expect_equal(guessCtype(void %>% .[[1]]),"___")
  expect_equal(guessCtypes(void),c("___","___"))
  guessCformats(void)
  v <- c("1",NA,"2")
  expect_equal(guessCtype(v),"Num")
  data <- data_frame(a = as.Date(c("2016-04-03", "2016-05-04")),
                     b = as.character(c("2016-04-03", "2016-05-04")),
                     c = as.factor(c("2016-04-03", "2016-05-04")))
  expect_true(all(map_lgl(data,isDate)))
  expect_equal(unique(map_chr(data,guessCtype)),"Dat")
  expect_equal(guessCtypes(data),c("Dat","Dat","Dat"))
  data <- sampleData("Cat-Dat-Yea-Num-Pct")
  expect_equal(guessFtype(data),"Cat-Dat-Num-Pct-Yea")

  guessCformats(data)

  expect_false("___" %in% availableCtypeIds(allowEmpty = FALSE))

  #ctypesToFtype
  expect_equal(ctypesToFtype(c("Num-Yea-Num"), as_string = TRUE),"Num2-Yea")
  expect_equal(ctypesToFtype("Num-Yea-Num-Yea-Cat-Yea-___", as_string = TRUE),"___-Cat-Num2-Yea3")
  #vectorized
  ctps <- c("Num-Cat-Cat", "Yea-Yea")
  expect_equal(ctypesToFtype(ctps, as_string = TRUE),c("Cat2-Num","Yea2"))


  ftype <- "Cat-Dat"
  d <- sampleData(ftype)
  ctypes <- guessCtypes(d)
  forceCtypes(d, ctypes)

  ftype <- "Cat-Dat-Yea-Num"
  d <- sampleData(ftype)
  #ctypes <- guessCtypes(d)
  ctypes <- c("Cat","Dat","Yea","Num")
  forceCtypes(d, ctypes)

  v <- sampleDat(20)
  parseDatetime(v,"Dat")

  v <- c("2000-01-04","2000-01-03","2000-01-04","2000-01-03","2000-01-02",
         "2000-01-01",NA,NA,"2000-01-03","2000-01-04",
         "2000-01-03","2000-01-02","2000-01-01","2000-01-03","2000-01-02",
         "2000-01-02","2000-01-02","2000-01-04","2000-01-02","2000-01-02")
  #format <- guess_formats(v, "ymd") # issue when multiple matches
  parseDatetime(v,"Dat")
})
