test_that("frType", {

  frtype <- frType("Num-Cat")

  expect_true(inherits(c(frType("Num"), "Cat"),"frType"))

  y <- frType("Cat")
  expect_true(frType("Cat") == y)

  x <- c("Cat-Num-Cat")
  fr <- frType(x)
  expect_equal(get_frGroup(fr), "Cat2-Num")
  expect_equal(frType_hdTypes(fr), hdType(c("Cat", "Num", "Cat")))

  x <- c("Cat-Num", "Cat-Num-Cat", "Cat")
  fr <- frType(x)
  expect_equal(get_frGroup(fr), c("Cat-Num","Cat2-Num", "Cat"))
  # With a vector of frtypes, hdtypes become a list
  expect_equal(frType_hdTypes(fr),
               list(
                 hdType(c("Cat", "Num")),
                 hdType(c("Cat", "Num", "Cat")),
                 hdType("Cat")
               ))
  f1 <- fr[1]
  get_frGroup(f1)
  get_frGroup(f1)


})
