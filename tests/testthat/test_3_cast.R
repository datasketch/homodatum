context("cast")

test_that("Cast Ctypes",{

  ctype <- "Cat"
  expect_equal(castable_ctype("Cat"), c("Uid","Oca","Txt","Bin"))
  ctypes <- availableCtypes()
  ctypes <- c("Cat","Num")
  castables <- castable_list(ctypes)
  randomCtype <- sample(castables[[1]],1)
  cs <- castable_ctypes()
  expect_equal(19,nrow(cs))
  nr <- cs %>% filter(from == ctypes[1], to == randomCtype) %>% nrow()
  if(ctypes[1] != randomCtype){
    expect_equal(1,nr)
  }

  d0 <- sampleData("Cat-Cat-Num-Pct-Gcd-Gnm")
  data <- d0[sample(names(d0),sample(1:5,1))]
  ctypes <- guessCtypes(data)

  ncomb <- (map_int(ctypes, function(c){
    castable_ctypes() %>% filter(from == c) %>% nrow()
    }) + 1) %>% reduce(`*`)
  # Make sure possible combinations (* of all possible values for each column)
  # Equals the number of return castable options
  expect_equal(nrow(castable_list(ctypes)), ncomb)

})
