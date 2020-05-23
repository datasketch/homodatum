test_that("Seq", {

  # Seq
  x <- c(c("Apple","Banana", "Banana", "Lemon"), NA)
  seqs <- new_Seq(x)
  seqs
  attr(seqs, "stats")
  attr(seqs, "categories")
  str(seqs)
  expect_equal(attr(seqs,"stats")$category, unique(x))
  expect_equal(attr(seqs,"stats")$n[2], 2)
  expect_equal(attr(seqs,"n_categories"), 3)


  #Seq(NULL)
  Seq(NA)

  # Accepts anything coercible from double()
  x <- Seq(c("1","0.2"))
  class(x)
  expect_true(inherits(x, "hd_Seq"))

  x <- Seq(c(1,1,2,2), order = c(2,1)) # TODO: Seq cannot be numbers
  x
  expect_equal(Seq_get_categories(x),c("1","2"))
  expect_equal(Seq_get_n_categories(x), 2)
  expect_equal(Seq_get_order(x), 2:1)

  ## TODO check order has the same type as categories

  stats <- Seq_get_stats(x)
  expect_equal(stats$n[1:2], as.vector(table(x)))

  a <- data.frame(myseqs = Seq(c("black", "white")), value = 1:2)
 tibble::tibble(a)
  ## TODO fix data.frame print

})
