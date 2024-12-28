#' WMS INSPIRE: Retrieve Map Images from the Basque Country (Pais Vasco) Based on Bounding Box Coordinates
#'
#' @importFrom sf st_as_sf st_transform st_coordinates
#'
#' @description
#' Fetches geotagged images from the Cadastre based on the provided bounding box coordinates.
#' This function determines the province (Bizkaia, Gipuzkoa, or Araba/Álava) using reverse
#' geocoding and fetches the appropriate data using specific WMS service functions for each
#' province. It ensures the coordinates fall within a single province and handles different
#' CRS inputs.
#'
#' @param x A numeric vector of bbox coordinates or an `sf` object defining the area to be covered.
#'   - A numeric vector of length 4 with the coordinates defining the bounding box:
#'     `c(xmin, ymin, xmax, ymax)`.
#' @param srs The spatial reference system to be used, typically an EPSG code (3857 or 25830).
#' @param what Specifies the layer to be extracted. Options include:
#'
#' For Bizkaia:
#' - `"parcel"`: CP.CadastralParcel
#' - `"zoning"`: CP.CadastralZoning
#' - `"address"`: AD.Address
#' - `"admunit"`: AU.AdministrativeUnit
#'
#' For Gipuzkoa:
#' - `"parcel"`: cp.CadastralParcel
#' - `"zoning"`: cp.CadastralZoning
#' - `"address"`: ad.Address
#' - `"admunit"`: au.AdministrativeUnit
#' - `"admbound"`: au.AdministrativeBoundary
#' - `"building"`: bu.building
#' - `"buother"`: bu.otherconstruction
#'
#' For Araba:
#' - `"parcel"`: CP.CadastralParcel
#' - `"admunit"`: AU.AdministrativeUnit
#' - `"admbound"`: AU.AdministrativeBoundary
#' - `"building"`: BU.Building
#'
#' @param styles Specifies the style of the WMS layer, see **Styles** for options.
#' @param id An identifier for the custom WMS service configuration.
#' @param crop Logical; if `TRUE`, the returned image will be cropped to the bbox. Default is `FALSE`.
#' @param options List; additional options for the WMS request.
#' @param verbose Logical; if `TRUE`, prints additional information about function operations.
#'   Useful for debugging. Default is `FALSE`.
#'
#' @return
#' Returns a [`SpatRaster`][terra::rast] object with either RGB or RGBA layers depending on the data.
#' Additionally, prints messages detailing the province detected and any potential issues with
#' the provided coordinates.
#'
#' @family INSPIRE
#' @family WMS
#' @family spatial data layers from Basque Country (Pais Vasco)
#'
#' @seealso
#' [mapSpain::esp_getTiles()], [mapSpain::esp_make_provider()], [terra::RGB()].
#' For advanced plotting, see [terra::plotRGB()] and [tidyterra::geom_spatraster_rgb()].
#'
#' @details
#' This function uses reverse geocoding to determine the province within the Basque Country from
#' the provided coordinates. Based on the identified province, it delegates the data fetching to
#' specific functions handling each province's data. It supports flexible input types and handles
#' geographical coordinate transformations internally if needed.
#'
#' # Styles
#' Various styles are available for each layer, which can enhance the visualization:
#'
#' Bizkaia:
#' - `"parcel"`: `"CP.CadastralParcel.BoundariesOnly"`, `"CP.CadastralParcel.ReferencePointOnly"`, `"CP.CadastralParcel.LabelOnReferencePoint"`, `"CP.CadastralParcel.Default"`, `"inspire_common:DEFAULT"`
#' - `"zoning"`: `"CP.CadastralZoning.Default"`, `"inspire_common:DEFAULT"`
#' - `"address"`: `"AD.Address.Default"`, `"inspire_common:DEFAULT"`
#' - `"admunit"`: `"AU.AdministrativeUnit.Default"`, `"inspire_common:DEFAULT"`
#'
#' Gipuzkoa:
#' - `"parcel"`: `"CP.CadastralParcel.BoundariesOnly"`, `"CP.CadastralParcel.ReferencePointOnly"`, `"CP.CadastralParcel.LabelOnReferencePoint"`, `"CP.CadastralParcel.Default"`, `"CP.CadastralParcel.ELFCadastre"`
#' - `"zoning"`: `"CP.CadastralZoning.Default"`, `"CP.CadastralZoning.ELFCadastre"`
#' - `"address"`: `"AD.Address.Default"`, `"AD.Address.Number.ELFCadastre"`
#' - `"admunit"`: `"AU.AdministrativeUnit.Default"`, `"AU.AdministrativeUnit.ELFCadastre"`
#' - `"admbound"`: `"AU.AdministrativeBoundary.Default"`, `"AU.AdministrativeBoundary.ELFCadastre"`
#' - `"building"`: `"BU.Building.Default"`, `"BU.Building.ELFCadastre"`
#' - `"buother"`: `"BU.OtherConstruction.Default"`,`"BU.OtherConstruction.ELFCadastre"`
#'
#' Araba:
#' - `"parcel"`: `"INSPIRE_CP:CP.CadastralParcel.Default"`, `"INSPIRE_CP:CP.CadastralParcel.ELFCadastre"`, `"INSPIRE_CP:CP.CadastralParcel.BoundariesOnly"`, `"INSPIRE_CP:CP.CadastralParcel.LabelOnReferencePoint"`, `"INSPIRE_CP:CP.CadastralParcel.ReferencePointOnly"`
#' - `"admunit"`: `"INSPIRE_AU:AU.AdministrativeUnit.Default"`
#' - `"admbound"`: `"INSPIRE_AU:AU.AdministrativeBoundary.Default"`
#' - `"building"`: `"INSPIRE_BU:BU.Building.Default"`
#'
#' @examples
#' \donttest{
#' library(mapSpain)
#' library(ggplot2)
#' library(terra)
#' library(tidyterra)
#'
#' # Fetching a building layer using specific bounding box coordinates (Araba)
#' pic_bu <- catreus_wms_get_layer(c(-298730.238481, 5288011.551711, -296360.690604, 5289922.477418),
#'   srs = 3857, what = "building", id = "layer_eus_1"
#' )
#'
#' # Displaying the fetched raster image
#' ggplot() +
#'   geom_spatraster_rgb(data = pic_bu)
#'
#' # Fetching an address layer using specific bounding box coordinates (Gipuzkoa)
#' pic_ad <- catreus_wms_get_layer(c(-221868.029226, 5358914.061417, -220750.137688, 5360872.760267),
#'   srs = 3857, what = "address", id = "layer_eus_2"
#' )
#'
#' # Displaying the fetched raster image
#' ggplot() +
#'   geom_spatraster_rgb(data = pic_ad)
#'
#' # Fetching a zoning layer using specific bounding box coordinates (Bizkaia)
#' pic_zo <- catreus_wms_get_layer(c(-339724.372213, 5356554.068169, -333915.158064, 5361598.912035),
#'   srs = 3857, what = "zoning", id = "layer_eus_3"
#' )
#'
#' # Displaying the fetched raster image
#' ggplot() +
#'   geom_spatraster_rgb(data = pic_zo)
#' }
#'
#' @export

catreus_wms_get_layer <- function(x, srs = NULL, what = c("parcel", "admunit", "admbound", "building", "zoning", "address", "buother"),
                                  id = "change_id", styles = NULL, verbose = FALSE, crop = FALSE, options = list()) {
  if (missing(what)) {
    what <- what[1]
  }

  # Validate that the value of `what` is within the allowed values.
  if (!what %in% c("parcel", "admunit", "admbound", "building", "zoning", "address", "buother")) {
    stop("The value of 'what' is not valid.")
  }

  if (inherits(x, "sf") || inherits(x, "sfc")) {
    # Convert the object into bbox
    bbox <- sf::st_bbox(x)
    coords <- c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    if (is.null(srs)) {
      srs <- sf::st_crs(x)$epsg
    }
  } else if (is.numeric(x) && length(x) == 4) {
    coords <- x
    if (is.null(srs)) {
      stop("Please provide a srs value")
    }
  } else {
    stop("The x parameter should be a numeric vector with length 4 or an sf/sfc object.")
  }

  if (srs == 3857) {
    crs <- 3857
  } else {
    crs <- 25830
  }
  coords_prov <- matrix(coords, ncol = 2, byrow = TRUE)
  sf_object <- st_as_sf(data.frame(x = coords_prov[, 1], y = coords_prov[, 2]), coords = c("x", "y"), crs = crs)
  transformed_sf <- st_transform(sf_object, crs = 4326)
  coords2 <- st_coordinates(transformed_sf)
  lat1 <- coords2[1, "Y"]
  long1 <- coords2[1, "X"]
  lat2 <- coords2[2, "Y"]
  long2 <- coords2[2, "X"]
  town_data1 <- tidygeocoder::reverse_geo(lat = lat1, long = long1, method = "osm", full_results = TRUE)
  town_data2 <- tidygeocoder::reverse_geo(lat = lat2, long = long2, method = "osm", full_results = TRUE)
  province1 <- town_data1$province
  province2 <- town_data2$province

  if ((province1 == "Bizkaia") & (province2 == "Bizkaia")) {
    if (!what %in% c("parcel", "admunit", "zoning", "address")) {
      stop("The value of 'what' is not valid for Bizkaia. Change it for 'parcel', 'admunit', 'zoning' or 'address'")
    }
    print("Coordinates of Bizkaia:")
    print(what)
    if (is.null(styles)) {
      styles <- "default"
    }
    catreus_bizk_wms_get_layer(coords, srs, what = what, styles = styles, id = id, crop = crop, options = options)
  } else if ((province1 == "Gipuzkoa") & (province2 == "Gipuzkoa")) {
    print("Coordinates of Gipuzkoa:")
    print(what)
    if (is.null(styles)) {
      styles <- "default"
    }
    catreus_gipu_wms_get_layer(coords, srs, what = what, styles = styles, id = id, crop = crop, options = options)
  } else if ((province1 == "Araba/\u00C1lava") & (province2 == "Araba/\u00C1lava")) {
    if (!what %in% c("parcel", "admunit", "admbound", "building")) {
      stop("The value of 'what' is not valid for Araba. Change it for 'parcel', 'admunit', 'admbound' or 'building'")
    }
    print("Coordinates of Araba/\u00C1lava:")
    print(what)
    catreus_arab_wms_get_layer(coords, srs, what = what, styles = styles, id = id, crop = crop, options = options)
  } else if (province1 != province2) {
    stop("This coordinates englobe 2 different province. ", province1, " and ", province2, ". Please select coordinates from 1 province.")
  } else {
    stop("This coordinates aren't from the Basque Country. Please change the bbox")
  }
}
