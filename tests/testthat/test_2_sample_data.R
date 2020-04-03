context("sample data")


test_that("Sample Column",{

  x <- diff(vctrs::vec_data(sampleDat(10, addNA = FALSE)))
  expect_true(all(x == 1))

  sampleDat(n = 100, rep= FALSE)

  sampleDat(n = 100, rep= TRUE)
  sampleDat(n = 6, rep= TRUE)
  # sampleWdy(n = 10, rep= TRUE)
  sampleDtm(n = 10, rep= TRUE) ## Todo test with rep
  sampleTxt(n = 10, rep= TRUE) ## Todo test with rep

  frtype <- "Cat"
  d <- sampleData(frtype)

  frtype <- "Dat"
  d <- sampleData(frtype, addNA = FALSE)

  frtype <- "Num"
  d <- sampleData(frtype)

  frtype <- "Cat-Dat"
  d <- sampleData(frtype)
  d
  frtype <- guess_frType(d)
  # forceCtypes(d, ctypes)


})

test_that("Sample Data", {

  t <- sampleData("Cat-Num")

  homodatum:::sampleCat(10)

  fr <- guess_frType(t)
  expect_equal(frType_str(fr),"Cat-Num")

  expect_equal(guess_frType(t, as_string = TRUE),c("Cat-Num"))

    # t <- sampleData("Uid-Cat-Num-___")
  # sampleData("Uid-Cat-Num-___-Img")

  t <- sampleData("Cat-Num", loremNames = FALSE, gt0 = FALSE)
  ## TODO... this test is a random value!!!
  # expect_true(any(t$b < 0))

  t <- sampleData("Cat-Num")
  ### TODO Add option to generate sampleData with given frGroup
  # expect_equal(guess_frType(t),ctypes)

  # Dat-Num

  t <- sampleData("Dat-Num", nrow = 10, addNA = FALSE)
  x <- diff(vctrs::vec_data(t[[1]]))
  expect_true(all(x == 1))
  expect_false(any(is.na(t[[1]])))

  ctypes <- "Gcd-Gnm-Glt-Gln"
  t <- sampleData(ctypes, scope = "world")

  t <- sampleData(ctypes, scope = "col_departments", loremNames = FALSE)
  expect_true(all(na.omit(t$b) %in% geodata::geodataCsv("col_departments")$name))

  # Test sample Bin

})



