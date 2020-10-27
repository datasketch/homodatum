test_that("Fringe IO works", {

  d <- tibble::tibble("Helloo X" = 1, "x 43" = as.Date("2020-04-21"))
  fr <- fringe(d, name = "Los Carros", mas = "fda")
  expect_true(fr$meta$mas == "fda")
  expect_true(fr$slug == "los-carros")

  dir <- tempdir(check = TRUE)
  # dir <- "~/Downloads/tmp"
  ### for some reason this tempdir clashes with write_csv(data, "")

  fringe_write(fr, path = dir, overwrite_dic = TRUE)
  fringe_write_json(fr, path = dir)
  fringe_write_xlsx(fr, path = dir)

  expect_true(all(file.exists(file.path(dir,
                                        c('los-carros.csv',
                                          'los-carros.dic.csv',
                                          'los-carros.json',
                                          'los-carros.meta.json',
                                          'los-carros.xlsx',
                                          'los-carros.yaml')))))

  path <- file.path(dir, fr$slug)

  fr2 <- fringe_read(path)

  expect_equal(fr$data, fr2$data)
  expect_equal(fr$dic, fr2$dic)
  expect_equal(fr$meta, fr2$meta)
  # expect_equal(fr, fr2)

  unlink(dir, recursive = TRUE)

})
