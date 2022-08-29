context("combinations")

test_that("frtype combinations work",{


  frt <- frType("Cat-Num")
  comb_frt <- frType_combine(frt)
  expect_equal(frType_combine(hdType(c("Cat","Num"))),comb_frt)

  expect_equal(length(comb_frt),2 ^ length(frType_hdTypes(frt)) - 1)

  hdtypes <- hdType(c(A = "Cat", B = "Num"))
  perm <- hdTypes_permute(hdtypes)
  expect_equal(perm[[1]], hdType(c(A = "Cat", B = "Num")))
  expect_equal(perm[[2]], hdType(c(B = "Num", A = "Cat")))

  hdtypes <- hdType(c(A = "Cat", B = "Cat", C = "Num"))
  sub <- sub_hdTypesVars(hdtypes, frtype = "Cat-Num")
  expect_equal(sub[[1]], hdType(c(A = "Cat", C = "Num")))
  expect_equal(sub[[2]], hdType(c(B = "Cat", C = "Num")))
  sub2 <- sub_hdTypesVars(hdtypes, frtype = frType("Cat-Num"))
  expect_equal(sub, sub2)
  #
  # sub <- sub_hdTypesVars(hdtypes, frtype = c("Cat","Cat-Num"))
  # expect_equal(sub[[2]], hdType(c(B = "Cat")))
  # expect_equal(sub[[4]], hdType(c(B = "Cat", C = "Num")))
  #
  # sub <- sub_hdTypesVars(hdtypes, group = "Cat2-Num")
  # frtypes <- lapply(sub, frType)
  # expect_equal(unique(lapply(frtypes, frType_group))[[1]], "Cat2-Num")

})

test_that("subFringe works", {

  # x <- sample_data("Cat-Num-Cat-Gnm", names = c("cat1", "num", "cat2", "gnm"))
  # fr <- fringe(x)
  # x <- sub_fringe_cols(fr, frtype = "Cat-Num")
  # sub_fringe_cols(fr, frtype = "Cat-Num", show_hdType = TRUE)
  #
  # sub_fringe_cols(fr, group = "Cat2-Gnm")
  # sub_fringe_cols(fr, group = "Cat2-Gnm", show_hdType = TRUE)


})


