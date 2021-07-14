test_that("Gnm, Gcd, Gln, Glt", {


  # Gnm
  x <- c(c("Bogota","Berlin", "New York"), NA)
  cities <- new_Gnm(x)
  attr(cities, "stats")
  attr(cities, "categories")
  str(cities)
  expect_true(all(attr(cities,"stats")$category %in% unique(x)))
  expect_equal(attr(cities,"stats")$n[1], 1)
  expect_equal(attr(cities,"n_categories"), 3)

  Gnm()
  x <- new_Gnm(c("Colombia", "Argentina", "Brazil"))
  vec <- c("Colombia", "Argentina", "Brazil")
  class(vec)
  vec_test <- as_Gnm(vec)

  expect_true("hd_Gnm" %in% class(vec_test))


  vec <- 1:10
  class(vec)
  vec_test <- as_Gnm(vec)

  expect_true("hd_Gnm" %in% class(vec_test))
  # TODO add option to TRIM (spaces, etc) Gnms, and to regroup/refactor

  x <- letters
  names(x) <- LETTERS
  cities <- new_Gnm(x)
  expect_equal(attr(cities,"stats")$names, c(LETTERS, NA))

  # Accepts anything coercible from double()
  # TODO ADD SCOPE FOR VALIDATION
  x <- Gnm(c("1","0.2")) # should fail with numbers?
  expect_true(inherits(x, "hd_Gnm"))


  # Glt
  x <- new_Glt(c(seq(0, 1, length.out = 4), NA))
  x
  str(x)
  expect_equal(Glt_get_stats(x)$min, 0)
  expect_equal(Glt_get_stats(x)$max, 1)
  expect_equivalent(vctrs::vec_cast(x, double()), c(seq(0, 1, length.out = 4), NA))

  expect_error(Glt(100))

  # Gln
  x <- new_Gln(c(seq(0, 1, length.out = 4), NA))

  expect_equal(Gln_get_stats(x)$min, 0)
  expect_equal(Gln_get_stats(x)$max, 1)
  expect_equivalent(vctrs::vec_cast(x, double()), c(seq(0, 1, length.out = 4), NA))

  expect_error(Gln(200))



})
