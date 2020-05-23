test_that("Txt", {

  # Txt

  hundred_years <- c(
    "Muchos años después, frente al pelotón de fusilamiento, el coronel Aureliano Buendía
    había de recordar aquella tarde remota en que su padre lo llevó a conocer el hielo.",
  "Macondo era entonces una aldea de 20 casas de barro y cañabrava construidas a la orilla
  de un río de aguas diáfanas que se precipitaban por un lecho de piedras pulidas, blancas
  y enormes como huevos prehistóricos.",
  "El mundo era tan reciente, que muchas cosas carecían de nombre, y para mencionarlas
  había que señalarlas con el dedo.")

  nwords(c('Hola Mundo', "!"))

  x <- c(hundred_years, NA)
  text <- new_Txt(x)
  text
  attr(text, "stats")
  str(text)
  expect_equal(attr(text,"stats")$chars, c(172,217,119,NA))
  expect_equal(attr(text,"stats")$words, c(28,37,20,0))

  # TODO add option to TRIM (spaces, etc) Cats, and to regroup/refactor

  #Cat(NULL)
  Txt(NA)

  # Accepts anything coercible from double()
  x <- Txt(c("1","0.2"))
  class(x)
  expect_true(inherits(x, "hd_Txt"))

  expect_error(Txt(c(1,1,2,2,3,3))) # Txt should not be numbers... or shoud it?

  a <- data.frame(idx = 1:3, sentence = Txt(hundred_years))
 tibble::tibble(a)

})
