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
  castable <- castable_list(ctypes)
  expect_equal(nrow(castable), ncomb)
  expect_equal(unique(map_chr(castable,class)),"character")


  ## Powerset of vars

  expect_equal(length(possibleCtypes(ctypes)), 2 ^ length(ctypes) - 1)
  possibleCtypes(ctypes, castable = TRUE)

  ## Permutations

  data <- d0

  permuteCtypes(ctypes)
  permuteCtypes(ctypes, names(data))


  l <- possibleSubdata(data, permute = TRUE) # TODO ctype casts

  whichSubdata(data, "Cat-Num")
  whichSubdata(data, "Cat-Num-Cat")


  # Cuántos son y porqué?
  # sum(map_dbl(1L:7L,function(x) factorial(x)*(2^x-1)))


})
