test_that("ATOM Buildings", {
  skip_on_cran()
  skip_if_offline()
  
  expect_message(catreus_bizk_atom_get_buildings_db_all(
    verbose = TRUE,
    cache_dir = tempdir()
  ))
  expect_message(catreus_bizk_atom_get_buildings("xyxghx"))
  
  s <- catreus_bizk_atom_get_buildings("053", verbose = TRUE)
  
  expect_s3_class(s, "sf")
})