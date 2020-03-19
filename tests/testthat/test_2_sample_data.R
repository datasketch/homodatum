context("sample data")

test_that("Sample Data", {

  t <- sampleData("Cat-Num")
  fr <- guess_frType(t)
  expect_equal(frType_str(fr),"Cat-Num")

  expect_equal(guess_frType(t, as_string = TRUE),c("Cat-Num"))
  #expect_error(sampleData("XXXXXX"))

  # ftype <- paste0(availableCtypeIds(),collapse="-")
  # sampleData(ftype)

  # t <- sampleData("Uid-Cat-Num-___")
  # sampleData("Uid-Cat-Num-___-Img")

  t <- sampleData("Cat-Num", loremNames = FALSE, gt0 = FALSE)
  ## TODO... this test is a random value!!!
  expect_true(any(t$b < 0))
  t <- sampleData("Cat-Num")
  ### TODO Add option to generate sampleData with given frGroup
  # expect_equal(guess_frType(t),ctypes)

  # ftype <- paste0(availableCtypeIds(),collapse = "-")
  # data <- sampleData(ftype, rep = TRUE)
  # data <- sampleData(ftype)

  sampleDat(n = 100, rep= TRUE)
  sampleDat(n = 6, rep= TRUE)
  # sampleWdy(n = 10, rep= TRUE)
  sampleDtm(n = 10, rep= TRUE) ## Todo test with rep
  sampleTxt(n = 10, rep= TRUE) ## Todo test with rep

  ctypes <- "Gcd-Gnm-Glt-Gln"
  t <- sampleData(ctypes, scope = "world")

  t <- sampleData(ctypes, scope = "col_departments", loremNames = FALSE)
  expect_true(all(na.omit(t$b) %in% geodata::geodataCsv("col_departments")$name))

  # Test sample Bin

})



