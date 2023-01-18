# test_that("hdTibble", {
#
#   library(dplyr)
#
#   data <- data.frame(
#     a = Cat(c("black", "white")),
#     b = Dat(seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2)),
#     c = Yea(2001:2002),
#     d = Num(runif(2)*10),
#     e = Pct(runif(2))
#   )
#   class(data)
#   data %>% filter(a == "black")
#   data %>% select(a,b)
#   f <- fringe(data)
#   f_data <- fringe_data(f)
#   class(f_data)
#
#   f_data %>% filter(a == "black")
#   f_data %>% select(a,b)
#
#   data <- tibble(
#     a = c("black", "white"),
#     b = seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = 2),
#     c = 2001:2002,
#     d = runif(2)*10,
#     e = runif(2)
#   )
#   f <- fringe(data)
#   f_data <- fringe_data(f)
#
#   f_data %>% filter(a == "black")
#   f_data %>% select(a,b)
#
#
# })
