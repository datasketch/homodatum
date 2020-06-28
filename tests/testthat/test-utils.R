test_that("multiplication works", {

  nm <- "P.U. (US$)"
  expect_equal(make_slug(nm),"p-u-us")
  expect_equal(col_ids_from_name(nm),"p_u_us")

  # Todo check unique colnames



})
