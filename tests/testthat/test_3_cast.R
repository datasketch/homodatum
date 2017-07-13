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

  #data <- sampleData()
  #cast_ctypes(data, to = newCtype)


})
