test_that("ATOM Cadastral Parcels", {
  skip_on_cran()
  skip_if_offline()

  expect_message(catreus_bizk_atom_get_parcels_db_all(
    verbose = TRUE,
    cache_dir = tempdir()
  ))
  expect_message(catreus_bizk_atom_get_parcels("xyxghx"))

  s <- catreus_bizk_atom_get_parcels("061", verbose = TRUE)

  expect_s3_class(s, "sf")
})
