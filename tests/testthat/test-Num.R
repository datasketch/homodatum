test_that("Num",{
  # Num

  Num()


  x <- new_Num(c(seq(0, 1, length.out = 4), NA))
  x
  str(x)
  expect_equal(Num_get_stats(x)$min, 0)
  expect_equal(Num_get_stats(x)$max, 1)

  vec_ptype2(new_Num(1), new_Num(3))

  vec_ptype2(new_Num(1.1), new_Num(3.2))

  vec_ptype2(new_Num(1), new_Num(2:3))



  vctrs::vec_cast(x, double())

  vec_cast(1, Num())
  vec_cast(Num(1.1), double())

  expect_equivalent(vctrs::vec_cast(x, double()), c(seq(0, 1, length.out = 4), NA))

  vec_c(Num(0.5), 1)

  c(Num(1),1)
  c("Num", hdType("Num"))

  vctrs::vec_ptype2(1, Num())
  vctrs::vec_ptype2(Num(),1)

  vctrs::vec_ptype_show(Num(), double(), Num())

  vctrs::vec_cast(1, Num())
  vctrs::vec_cast(Num(1), double())

  # vctrs::vec_cast(Num(1), character()) ## no need, replaced with as.character
  vctrs::vec_cast("1", Num())

  data <- data.frame(
    e = Num(runif(2))
  )
  # write.csv(data,"~/Downloads/test.csv")

  as_Num("1")


  x <- Num(1.1)
  as.character(x)

  x <- c("4,59", "5,38", "10,78", NA, "2")
  expect_equal(guess_hdType(x), hdType("Num"))
  expect_true(has_decimal_comma(x))
  expect_true(is_Num(Num(x)))

  x <- c("343.755,08", "5.380,00", NA, "21.555,11", "1.550.000")
  expect_equal(guess_hdType(x), hdType("Num"))
  expect_true(has_decimal_comma(x))
  expect_true(is_Num(Num(x)))

})




# new_percent <- function(x = double()) {
#   vec_assert(x, double())
#   new_vctr(x, class = "vctrs_percent")
# }
# x <- new_percent(c(seq(0, 1, length.out = 4), NA))
# d <- data.frame(x)
# d
# as.character.vctrs_percent <- function(x) as.character(vec_data(x))
# write.csv(d, "~/Downloads/tmp.csv") # Error: Can't convert <vctrs_percent> to <character>.
#
# vctrs::vec_ptype2(1, new_percent())
# vctrs::vec_ptype2(1.1, new_percent())
#
# percent <- function(x = double()) {
#   x <- vec_cast(x, double())
#   new_percent(x)
# }
#
# vec_ptype2(percent(), percent())
#
# vec_ptype2("bogus", percent())
# #> Error: Can't combine <character> and <vctrs_percent>.
# vec_ptype2(percent(), NA)
# #> <vctrs_percent[0]>
# vec_ptype2(NA, percent())
# #> <vctrs_percent[0]>
#
# vec_ptype2(1, percent())
# vec_ptype2(1.1, percent())
#
# percent(0.3)
#
# vec_ptype2.vctrs_percent.vctrs_percent <- function(x, y, ...) new_percent()
# vec_ptype2(percent(1), percent(1))
#
# vec_ptype2.vctrs_percent.double <- function(x, y, ...) double()
# vec_ptype2.double.vctrs_percent <- function(x, y, ...) double()
#
# vec_ptype2(1, percent())
# vec_ptype2(1.1, percent())
#
# vec_ptype_show(percent(), double(), percent())





#' library(vctrs)
#' vec_ptype2(Num(),Num())
#' vec_ptype2(1.1,Num())
#' vec_ptype2(1.1,Num())
#'
#' new_Num <- function(x, skip_stats = FALSE){
#'   # vctrs::vec_assert(x, double())
#'   stats <- NULL
#'   if(length(x) == 0)
#'     skip_stats <- TRUE
#'   if(!skip_stats){
#'     stats <- list(
#'       min = min(x, na.rm = TRUE),
#'       max = max(x, na.rm = TRUE)
#'     )
#'   }
#'   vctrs::new_vctr(x, stats = stats, class = "hd_Num")
#' }
#'
#' #' @export
#' Num <- function(x = double()) {
#'   x <- vctrs::vec_cast(x, double())
#'   new_Num(x)
#' }
#'
#' vec_ptype2(1, Num())
#' vec_ptype2(1.1, Num())
#' # A Num combined with a Num returns a Num
#' vec_ptype2.hd_Num.hd_Num <- function(x, y, ...) new_Num()
#' vec_ptype2.hd_Num.double <- function(x, y, ...) double()
#' vec_ptype2.double.hd_Num <- function(x, y, ...) double()
#' vec_ptype2(1, Num())
#' vec_ptype2(1.1, Num())


