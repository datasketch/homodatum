context("ftypes")

test_that("Ftypes",{

  f <- ftype("Cat")
  expect_true(is_ftype(f))
   #To access object attributes,  my.object$attribute.name

  f <- ftype("Cat-Cat")
  expect_true(is_ftype(f))

  ctypes <- "Cat-Num-Cat-Dat"
  ctype(ctypes)

})
