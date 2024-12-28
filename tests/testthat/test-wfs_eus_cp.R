test_that("BBOX Check errors", {
  expect_error(catreus_wfs_get_parcels_bbox(x = "1234"))
  expect_error(catreus_wfs_get_parcels_bbox(c("1234", "a", "3", "4")))
  expect_error(catreus_wfs_get_parcels_bbox(c(1, 2, 3)))
  expect_error(catreus_wfs_get_parcels_bbox(c(1, 2, 3, 4)))
})


test_that("BBOX Check projections", {
  skip_on_cran()

  expect_error(catreus_wfs_get_parcels_bbox(c(42.331139, -3.721619, 42.346365, -3.685913),
    srs = 25829, count = 10
  ))

  expect_message(catreus_wfs_get_parcels_bbox(
    c(43.312, -2.994, 43.313, -2.993),
    srs = 25829,
    verbose = TRUE,
    count = 5
  ))

  obj <- catreus_wfs_get_parcels_bbox(c(43.026899, -2.433164, 43.308497, -2.063099),
    srs = 25830, count = 10
  )

  expect_true(sf::st_crs(obj) == sf::st_crs(25830))
  expect_true(nrow(obj) == 10)


  # test conversion
  testconv <- get_sf_from_bbox(obj[1, ])
  expect_identical(obj[1, ], testconv)

  # Convert to spatial object

  bbox <- get_sf_from_bbox(
    c(525255.750142, 4742699.503583, 527690.033671, 4744386.075341),
    srs = 25830
  )
  expect_s3_class(bbox, "sfc")

  obj2 <- catreus_wfs_get_parcels_bbox(bbox)
  expect_true(sf::st_crs(obj2) == sf::st_crs(25830))

  # Transform object to geographic coords
  bbox2 <- sf::st_transform(obj2[1, ], 25830)
  expect_false(sf::st_is_longlat(bbox2))
  expect_s3_class(bbox2, "sf")

  obj3 <- catreus_wfs_get_parcels_bbox(bbox2)

  expect_false(sf::st_is_longlat(obj3))
  expect_true(sf::st_crs(obj3) == sf::st_crs(25830))

  # BBox with coordinates

  vec <- as.double(sf::st_bbox(obj3[1, ]))

  obj4 <- catreus_wfs_get_parcels_bbox(vec, srs = 25830)

  expect_false(sf::st_is_longlat(obj4))
  expect_true(sf::st_crs(obj4) == sf::st_crs(25830))
})
