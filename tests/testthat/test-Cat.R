test_that("Cat", {

  # Cat
  x <- c(c("Apple","Banana", "Banana", "Lemons"), NA)
  cats <- new_Cat(x)
  attr(cats, "stats")
  attr(cats, "categories")
  str(cats)
  expect_equal(attr(cats,"stats")$summary$category, unique(x))
  expect_equal(attr(cats,"stats")$summary$n[2], 2)
  expect_equal(attr(cats,"stats")$n_unique, 3)
  expect_equal(attr(cats,"n_categories"), 3)

  # TODO add option to TRIM (spaces, etc) Cats, and to regroup/refactor

  x <- letters
  names(x) <- LETTERS
  cats <- new_Cat(x)
  expect_equal(attr(cats,"stats")$summary$names, c(LETTERS, NA))


  #Cat(NULL)
  Cat(NA)

  # Accepts anything coercible from double()
  x <- Cat(c("1","0.2"))
  class(x)
  expect_true(inherits(x, "hd_Cat"))

  c("x", Cat("y"))
  c(Cat("x"), "y")

  as.character(Cat(x))

  vctrs::vec_cast(Cat(c("c","d")), character())
  vctrs::vec_cast(c("c","d"), new_Bin())

  x <- Cat(c(1,1,2,2,3,3))
  expect_equal(Cat_get_categories(x),c("1","2","3"))
  expect_equal(Cat_get_n_categories(x), 3)
  stats <- Cat_get_stats(x)
  expect_equal(stats$summary$n[1:3], as.vector(table(x)))

  a <- data.frame(mycats = Cat(c("black", "white")), value = 1:2)
  tibble::tibble(a)

})
