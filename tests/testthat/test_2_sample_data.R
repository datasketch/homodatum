context("sample data")


test_that("Sample Column",{

  sampleDat(10, addNA = FALSE)

  x <- diff(vctrs::vec_data(sampleDat(10, addNA = FALSE)))
  expect_true(all(x == 1))

  sampleDat(n = 100, rep= FALSE)

  sampleDat(n = 100, rep= TRUE)
  sampleDat(n = 6, rep= TRUE)
  # sampleWdy(n = 10, rep= TRUE)
  sampleDtm(n = 10, rep= TRUE) ## Todo test with rep
  sampleTxt(n = 10, rep= TRUE) ## Todo test with rep

  frtype <- "Cat"
  d <- sample_data(frtype)

  frtype <- "Dat"
  d <- sample_data(frtype, addNA = FALSE)

  frtype <- "Num"
  d <- sample_data(frtype)

  frtype <- "Cat-Dat"
  d <- sample_data(frtype)
  d
  frtype <- guess_frType(d)
  # forceCtypes(d, ctypes)


})

test_that("Sample Data", {

  t <- sample_data("Cat-Num")

  homodatum:::sampleCat(10)



  fr <- guess_frType(t)
  expect_equal(frType_str(fr),"Cat-Num")

  expect_equal(guess_frType(t, as_string = TRUE),c("Cat-Num"))

    # t <- sample_data("Uid-Cat-Num-___")
  # sample_data("Uid-Cat-Num-___-Img")

  t <- sample_data("Cat-Num", loremNames = FALSE, gt0 = FALSE)
  ## TODO... this test is a random value!!!
  # expect_true(any(t$b < 0))

  t <- sample_data("Cat-Num", names = c("Name1","Name2"), loremNames = TRUE)
  expect_equal(names(t), c("Name1","Name2"))


  ### TODO Add option to generate sampleData with given frGroup
  # expect_equal(guess_frType(t),ctypes)

  # Dat-Num

  t <- sample_data("Dat-Num", nrow = 10, addNA = FALSE)
  x <- diff(vctrs::vec_data(t[[1]]))
  expect_true(all(x == 1))
  expect_false(any(is.na(t[[1]])))

  frtype <- "Gcd-Gnm-Glt-Gln"
  t <- sample_data(frtype, scope = "world")

  expect_equal(hdtibble_frType(t), frType(frtype))

  sample_data("Gln", scope = "col_departments", loremNames = FALSE)

  t <- sample_data(frtype, scope = "col_departments", loremNames = FALSE)
  expect_true(all(na.omit(t$b) %in% geodata::geodataCsv("col_departments")$name))

  data <- sample_data('Gnm-Num')
  f <- homodatum::fringe(data)

  # Test sample Bin

})



