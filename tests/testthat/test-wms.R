test_that("Check error", {
  expect_error(catreus_wms_get_layer(c(-221868.029226,5358914.061417,-220750.137688,5360872.760267),
                                     srs = 3857,
                                     what = "aa"))
})

test_that("Check tiles", {
  skip_on_cran()
  skip_on_os("linux")
  skip_if_offline()
  
  obj <- catreus_wms_get_layer(c(-298730.238481,5288011.551711,-296360.690604,5289922.477418), 
                               srs = 3857, what = "building")
  
  expect_s4_class(obj, "SpatRaster")
  
  
  # test crop
  objcrop <- catreus_wms_get_layer(c(-221868.029226,5358914.061417,-220750.137688,5360872.760267), 
                                srs= 3857, what = "address", crop = TRUE)
  
  
  expect_true(terra::nrow(obj) > terra::nrow(objcrop))
  
  
  # Convert to spatial object
  bbox <- get_sf_from_bbox(
    c(-221868.029226,5358914.061417,-220750.137688,5360872.760267),
    3857
  )
  expect_s3_class(bbox, "sfc")
  
  obj2 <- catreus_wms_get_layer(bbox)
  
  expect_s4_class(obj2, "SpatRaster")
  
  
  # With styles
  obj3 <- catreus_wms_get_layer(c(-339724.372213,5356554.068169,-333915.158064,5361598.912035),
                                srs = 3857,
                                what = "parcel",
                                styles = "CP.CadastralParcel.Default", id = "test-6",
                                options = list(version = "1.3.0"))
  
  expect_s4_class(obj3, "SpatRaster")
})