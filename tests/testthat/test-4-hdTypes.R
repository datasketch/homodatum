
test_that("create hdTypes",{

  library(readr)

  # TODO still need void ctype?
  # void =tibble::tibble(col1 = character(0), col2 = character(0))
  # expect_equal(guessCtype(void[1]),"___")
  # expect_equal(guessCtype(void %>% .[[1]]),"___")
  # expect_equal(guessCtypes(void),c("___","___"))
  # guessCformats(void)


  # expect_true(inherits(c(hdType("Num"), "Cat"),"hdType"))
  expect_true(inherits(c(hdType("Num"), "Cat"),"character"))
  ## TODO check coercion rules
  expect_true(inherits(c("Num", hdType("Cat")),"character"))
  expect_true(inherits(c(hdType("Num"), hdType("Cat")),"hdType"))





  # Data Frames

  # data <-tibble::tibble(a = as.Date(c("2016-04-03", "2016-05-04")),
  #                    b = as.character(c("2016-04-03", "2016-05-04")),
  #                    c = as.factor(c("2016-04-03", "2016-05-04")))
  # expect_true(all(purrr::map_lgl(data,isDate)))

  # expect_equal(unname(unique(vctrs::vec_c(!!!purrr::map(data,guess_hdType)))),hdType("Dat"))


  data <- data.frame(
    a = Cat(c("black", "white")),
    b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
    c = Yea(2001:2002),
    d = Num(runif(2)*10),
    e = Pct(runif(2))
  )

  expect_true(inherits(guess_hdType(data$a),"hdType"))

  # data <- sample_data("Cat-Dat-Yea-Num-Pct")
  hdTypes <- c(a = "Cat",b = "Dat", c = "Yea", d = "Num", e = "Pct")
  expect_equal(purrr::map_chr(data, guess_hdType),hdTypes)

  expect_equal(as.character(hdTypes), c("Cat", "Dat", "Yea", "Num", "Pct"))


  # expect_equal(guessFtype(data),"Cat-Dat-Yea-Num-Pct")

  # TODO check formats
  # guessCformats(data)

  #expect_false("___" %in% availableCtypeIds(allowEmpty = FALSE))

})


# test_that("Cast hdType",{
#
#   c(hdType("Num"),"Num")
#   c("Num", hdType("Num"))
#
#   vctrs::vec_ptype2("Cat", hdType())
#   vctrs::vec_ptype2(hdType(),"Num")
#
#   vctrs::vec_ptype_show(hdType(), character(), hdType())
#
#   vctrs::vec_cast("Num", hdType())
#   h <- hdType("Cat")
#   vctrs::vec_data(h)
#   vctrs::vec_cast(hdType("Cat"), character())
#
#   hdType("Cat") == "Cat"
#
#   d <- data.frame(x = hdType(c("Num", "Cat")), y = 1:2)
#   #readr::write_csv(d,"test.csv")
#
# })

# test_that("write hdTypes",{
#
#   data <- data.frame(
#     a = Cat(c("black", "white")),
#     b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
#     c = Yea(2001:2002),
#     d = Num(runif(2)*10),
#     e = Pct(runif(2))
#   )
#   data_str <- readr::write_csv(data,"test.csv") %>% tibble::as_tibble()
#   str(data_str)
#   test <- readr::read_csv("test.csv",
#                           col_types = readr::cols(.default = "c"))
#   unlink("test.csv")
#   #expect_equivalent(data_str, test)
#
# })



test_that("frType",{

  # frType

  frstr <- "Num-Yea-Num-Cat-Yea-Num"
  expect_equal(get_frGroup(frstr), "Cat-Num3-Yea2")
  expect_true(inherits(frType(frstr),"frType"))
  expect_equal(get_frGroup("Num-Yea-Num"),"Num2-Yea")


  #ctypesToFtype
  ## TODO weird behavior with sort
  # 1/1 mismatches
  # x[1]: "Cat-Num2-Yea3-___"
  # y[1]: "___-Cat-Num2-Yea3"
  # frstr <- "Num-Yea-Num-Yea-Cat-Yea-___"
  # expect_equal(get_frGroup(frstr),"___-Cat-Num2-Yea3")

  #vectorized
  frts <- c("Num-Cat-Cat", "Yea-Yea")
  fr <- frType(frts)
  expect_equal(get_frGroup(fr),c("Cat2-Num","Yea2"))

  ## Sample Data



  ftype <- "Cat-Dat-Yea-Num"
  d <- sample_data(ftype)
  #ctypes <- guessCtypes(d)
  hdtypes <- c("Cat","Dat","Yea","Num")

  frtype <- guess_frType(d)
  guess_frType(d)

  df <- data.frame(
    a = c("black", "white"),
    b = seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2),
    c = 2001:2002,
    d = runif(2)*10,
    e = runif(2)
  )
  frtype <- "Cat-Dat-Yea-Num-Pct"
  dd <- hdtibble(df, frtype)
  expect_equal(frtype, frType_str(dd))
  frtype <- "Cat-Dat-Num-Pct-Pct"
  dd <- hdtibble(df, frtype)
  expect_equal(frtype, frType_str(dd))


  ## Test coercion to base types

  d <- sample_data("Cat-Dat-Yea-Num-Pct")

  expect_true("character" %in% class(as_baseType(d[[1]])))
  expect_true("Date" %in% class(as_baseType(d[[2]])))
  expect_true("integer" %in% class(as_baseType(d[[3]])))
  expect_true("numeric" %in% class(as_baseType(d[[4]])))
  ## TODO check coercion to base types for all other hdTypes

})
