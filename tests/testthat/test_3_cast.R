context("cast")

test_that("Cast Ctypes",{

  # All possible casts: from -> to
  cs <- castable_ctypes()
  expect_equal(19,nrow(cs))

  ctype <- "Cat"
  expect_equal(castable_ctype("Cat"), c("Uid","Oca","Txt","Bin"))
  ctypes <- availableCtypes()

  # All possible casts for given ctypes
  ctypes <- c("Cat","Num","Pct")
  castables <- castable_list(ctypes)

  ctypes <- c("Cat","Num")
  castables <- castable_list(ctypes)

  randomCtype <- sample(castables[[1]],1)
  randomCtype
  # Test Castables for Cat
  nr <- cs %>% filter(from == ctypes[1], to == randomCtype) %>% nrow()
  if(ctypes[1] != randomCtype){
    expect_equal(1,nr)
  }


  # Test possible ctypes

  ctypes <- c("Cat","Num")
  comb <- possibleCtypes(ctypes, combine = TRUE)
  expect_equal(length(comb),2 ^ length(ctypes) - 1)

  ncomb <- function(ctypes){
    (map_int(ctypes, function(c){
      castable_ctypes() %>% filter(from == c) %>% nrow()
    }) + 1) %>% reduce(`*`)
  }
  ncomb(ctypes)
  castables <- possibleCtypes(ctypes, castable = TRUE)
  expect_equal(nrow(castables), ncomb(ctypes))

  castableCombs <- possibleCtypes(ctypes, castable = TRUE, combine = TRUE)
  expect_equal(length(castableCombs), 2 ^ length(ctypes) - 1)
  expect_equal(
    map(castableCombs,nrow),
    map(strsplit(names(castableCombs),"-"),ncomb)
  )

  # Test with a Random Ctype

  d0 <- sampleData("Cat-Cat-Num-Pct-Gcd-Gnm")
  data <- d0[sample(names(d0),sample(1:5,1))]
  ctypes <- guessCtypes(data)

  # Make sure possible combinations (* of all possible values for each column)
  # Equals the number of return castable options
  castable <- castable_list(ctypes)
  expect_equal(nrow(castable), ncomb)
  expect_equal(unique(map_chr(castable,class)),"character")


  ## Permutations

  ctypes1 <- c("Cat","Num")
  names(ctypes1) <- LETTERS[1:length(ctypes)]

  ctypes2 <- c("Cat","Num")
  expect_equal(permuteCtypes(ctypes1), permuteCtypes(ctypes2, nms = LETTERS[1:length(ctypes2)]))

  # Possible Named Ctypes

  namedCtypes <- c("Cat","Num","Pct")
  names(namedCtypes) <- LETTERS[1:length(namedCtypes)]
  possibleNamedCtypes(namedCtypes)
  possibleNamedCtypes(namedCtypes, castable = TRUE)


  # Possible Subdata

  data <- sampleData("Num-Cat-Oca", loremNames = FALSE)
  #whichSubdata(data)
  outCtypes <- c("Num","Cat","Cat")
  whichSubdata(data, castable = FALSE, outCtypes = outCtypes)
  whichSubdata(data, castable = TRUE, outCtypes = outCtypes)
  outCtypes <- c("Num","Cat","Oca")
  whichSubdata(data, castable = FALSE, outCtypes = outCtypes)
  whichSubdata(data, castable = TRUE, outCtypes = outCtypes)

  #whichSubdata(data, "Cat-Num-Cat")


  # Cuántos son y porqué?
  # sum(map_dbl(1L:7L,function(x) factorial(x)*(2^x-1)))


})
