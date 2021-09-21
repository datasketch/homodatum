test_that("Fringe verbs: select", {

  x <- sample_data("Cat-Cat-Num", names = c('first', 'second', 'third'))
  f <- fringe(x)



  labels <- c("third", "first")
  f1 <- fr_select(f, labels = labels)

  ids <- c("third", "first")
  f2 <- fr_select(f, ids = ids)

  expect_equal(f1, f2)
  expect_error(fr_select(f, ids = 1:3), "ids not found in data")

  dic <- create_dic(x)
  names(x) <- letters[1:3]
  dic$id <- names(x)

  f <- fringe(x, dic = dic)
  fr_select(f, ids = c("a", "b"))



  # data_url <- "https://datosabiertos.planificacion.gob.ec/dataset/29e91f72-6b69-4db1-b21b-d6cfe97472a7/resource/1116feb5-87b0-4d62-a144-aeff26435efe/download/reporte_estadistico_afiliados_julio_2021.csv"
  # default_data <- pdaeviz:::pdea_read_csv(data_url)
  #
  #
  # names(default_data) <- letters[1:11]
  # dic <- homodatum::create_dic(default_data)
  # dic$id <- names(default_data)
  #
  # f <- homodatum::fringe(default_data, dic = dic)
  # homodatum::fr_select(f, ids = c("a", "b"))
  #



})
