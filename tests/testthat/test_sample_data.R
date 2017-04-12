context("sample data")

test_that("Sample Data", {
  t <- sampleData("Cat-Num")
  identical(guessCtypes(t),c("Cat","Num"))
  expect_equal(guessCtypes(t),c("Cat","Num"))
  #expect_error(sampleData("XXXXXX"))

  ftype <- paste0(availableCtypeIds(),collapse="-")
  sampleData(ftype)

  t <- sampleData("Uid-Cat-Num-___")
  sampleData("Uid-Cat-Num-___-Img")

  t <- sampleData("Cat-Num", loremNames = FALSE, gt0 = FALSE)
  expect_true(any(t$b < 0))
  t <- sampleData("Cat-NumP")
  ctypes <- paste0(paste0(c("Cat","Num"),c("",ncol(t)-1)),collapse="-")
  expect_equal(guessFtype(t),ctypes)

  ftype <- paste0(availableCtypeIds(),collapse = "-")
  data <- sampleData(ftype, rep = TRUE)
  data <- sampleData(ftype)

  sampleDat(n = 100, rep= TRUE)
  sampleDat(n = 6, rep= TRUE)
  sampleWdy(n = 10, rep= TRUE)
  sampleDtm(n = 10, rep= TRUE) ## Todo test with rep
  sampleTxt(n = 10, rep= TRUE) ## Todo test with rep

})



