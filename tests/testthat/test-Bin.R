test_that("Bin", {

  # Bin
  x <- c(c("Apple","Banana", "Banana"), NA)
  bins <- new_Bin(x)
  attr(bins, "stats")
  attr(bins, "categories")
  str(bins)
  expect_equal(attr(bins,"stats")$category, unique(x))
  expect_equal(attr(bins,"stats")$n[2], 2)
  expect_equal(attr(bins,"n_categories"), 2)

  # TODO add option to TRIM (spaces, etc) Bins, and to regroup/refactor

  ## TODO Bin should be exactly 2 categories? or could be less?
  expect_error(Bin(letters[1:3]))

  #Bin(NULL)
  Bin(NA)

  # Accepts anything coercible from double()
  x <- Bin(c("1","0.2"))
  # class(x)
  expect_true(inherits(x, "hd_Bin"))

  #vctrs::vec_ptype_show(Bin(), character(), Bin())

  x <- Bin(c(1,1,2,2))

  vctrs::vec_cast(Bin(c("c","d")), character())
  vctrs::vec_cast(c("c","d"), new_Bin())

  as.character(x)

  expect_equal(Bin_get_categories(x),c("1","2"))
  expect_equal(Bin_get_n_categories(x), 2)
  stats <- Bin_get_stats(x)
  expect_equal(stats$n[1:2], as.vector(table(x)))

  a <- data.frame(mybins = Bin(c("black", "white")), value = 1:2)
  tibble::tibble(a)

})
