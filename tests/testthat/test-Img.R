test_that("Cat", {

  # Img
  x <- c(c("Apple.png","Banana.png", "Banana.png"), NA)
  imgs <- new_Img(x)
  attr(imgs, "format")
  expect_true(inherits(imgs, "hd_Img"))
  expect_equal(attr(imgs,"format"), "png")

  imgs
  as.character(imgs)

  #Img(NULL)
  Img(NA)

  # Does not accepts pics without extensions
  y <- c("1","0.XXX")
  expect_error(Img(y), "Unknown image formats")


  a <- data.frame(myimgs = Img(c("black.png", "white.png")), value = 1:2)
  d <- tibble::tibble(a)
  write.csv(d, "")

})
