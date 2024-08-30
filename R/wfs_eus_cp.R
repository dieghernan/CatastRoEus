#' Retrieve Parcel Data in Pais Vasco Based on Bounding Box Coordinates
#' 
#' @importFrom sf st_as_sf st_transform st_coordinates st_buffer
#' @importFrom sf st_buffer
#' @importFrom magrittr %>%
#'
#' @description
#' This function fetches parcel data within a specified bounding box. It first checks if the provided coordinates are valid,
#' determines the province based on these coordinates using reverse geocoding, and fetches parcel data for
#' Bizkaia, Gipuzkoa, or Araba/Álava accordingly. It supports transformations between geographical and UTM coordinate systems,
#' handles multiple Coordinate Reference Systems (CRS), and manages different minimum counts for returned records.
#'
#' @param x Bounding box coordinates or a spatial object, which could be:
#'   - A numeric vector of length 4 with the coordinates defining the bounding box:
#'     `c(latitude1, longitude1, latitude2, longitude2)`.
#'   - An `sf/sfc` object from the \CRANpkg{sf} package.
#' @param srs Spatial Reference System (SRS) or Coordinate Reference System (CRS) code 
#'   to be used in the query. For best results, ensure the coordinates are in ETRS89
#'   (EPSG:25830) or WGS84 (EPSG:4326) when using latitude and longitude.
#' @param verbose Logical; if `TRUE`, additional information about function operations 
#'   is printed. Useful for debugging. Default is `FALSE`.
#' @param count Integer specifying the maximum number of parcel records to return.
#'   Default is 10.
#'
#' @return Depending on the input and geographic location, this function may return:
#'   - A message detailing the success of the query and the province fetched.
#'   - A warning message if the bounding box spans multiple provinces.
#'   - An error message if the coordinates are not within Pais Vasco.
#'
#' @seealso \code{\link[sf:st_bbox]{st_bbox}}, which is used to manage spatial bounding boxes.
#'
#' @details
#' The function uses reverse geocoding to identify the province within the Basque Country.
#' It delegates data fetching to province-specific functions, which handle data extraction based on the 
#' provided geographic bounds. The function adapts dynamically to coordinate system inputs and provides options 
#' for detailed logging to facilitate debugging and verification of operations.
#'
#' @examples
#' \donttest{
#' 
#' library(mapSpain)
#' library(dplyr)
#' library(sf)
#' 
#' # Define bounding box coordinates for an urban location in Gipuzkoa
#' coords_gipuzkoa <- c(582745.070132,4795611.169048,584249.337348,4796830.604835)
#'
#' # Fetch parcel data using the bounding box
#' parcels_gipuzkoa <- catreus_wfs_get_parcels_bbox(coords_gipuzkoa, srs = 25830)
#'
#' library(ggplot2)
#' # Plot the parcels data
#' ggplot(parcels_gipuzkoa) +
#'   geom_sf() + ggtitle("Parcels Data for Gipuzkoa")
#'
#' # Define bounding box coordinates for a location in Bizkaia
#' coords_bizkaia <- c(504218.816311, 4788948.595082, 505374.746026, 4789719.963173)
#' 
#' # Fetch parcel data using the bounding box
#' parcels_bizkaia <- catreus_wfs_get_parcels_bbox(coords_bizkaia, srs = 25830)
#' 
#' library(ggplot2)
#' # Plot the parcels data for Bizkaia
#' ggplot(parcels_bizkaia) +
#'   geom_sf() + ggtitle("Parcels Data for Bizkaia")
#'
#' 
#' 
#' # Fetch parcel data using the bounding box
#' vitoria <- esp_get_capimun(munic = "Vitoria") %>%
#'   st_transform(4326) %>%
#'   st_buffer(300)
#'   
#' parcels_araba <- catreus_wfs_get_parcels_bbox(vitoria)
#' ggplot(parcels_araba) + geom_sf() + ggtitle("Parcels Data for Vitoria")
#' 
#' }
#'   
#' @export

catreus_wfs_get_parcels_bbox <- function(x, srs = NULL, 
                                         verbose = FALSE, count = NULL) {
  coords2 <- NULL
  min_count <- 500
  if (verbose == TRUE){
    message("Beginning analytics")
  }
  
  if (inherits(x, "sf") || inherits(x, "sfc")) {
    # Convertir el objeto sf a bbox
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
  
  if ((coords[2] < 90) & (coords[2] > -90)) {
    if ((coords[1] > 10) & (coords[3]>10)){
      lat1 <- coords[1]
      long1 <- coords[2]
      lat2 <- coords[3]
      long2 <- coords[4]
    }
    else{
      lat1 <- coords[2]
      long1 <- coords[1]
      lat2 <- coords[4]
      long2 <- coords[3]
      coords <- c(lat1, long1, lat2, long2)
    }
  }
  else{
    crs = 25830
    coords_prov <- matrix(coords, ncol = 2, byrow = TRUE)
    sf_object <- st_as_sf(data.frame(x = coords_prov[, 1], y = coords_prov[, 2]), coords = c("x", "y"), crs = crs)
    transformed_sf_4326 <- st_transform(sf_object, crs = 4326)
    coords_4326 <- st_coordinates(transformed_sf_4326)
    lat1 = coords_4326[1, "Y"]
    long1 = coords_4326[1, "X"]
    lat2 = coords_4326[2, "Y"]
    long2 = coords_4326[2, "X"]
    coords2 = c(lat1,long1,lat2,long2)
  }
  
  town_data1 = tidygeocoder::reverse_geo(lat = lat1, long = long1, method = "osm", full_results = TRUE)
  town_data2 = tidygeocoder::reverse_geo(lat = lat2, long = long2, method = "osm", full_results = TRUE)
  
  if (!"province" %in% names(town_data1) || !"province" %in% names(town_data2)) {
    stop("No address could be determined from the provided coordinates.")
  }
  
  province1 <- town_data1$province
  province2 <- town_data2$province
  
  if (is.null(province1) || is.null(province2)) {
    stop("No province could be determined from the provided coordinates.")
  }
  else if ((province1 == "Bizkaia") & (province2 == "Bizkaia")){
    print("Province of Bizkaia:")
    print("-------------------------------")
    if (srs != 25830){
      srs = 25830
    }
    if (is.null(coords2)){
      catreus_bizk_wfs_get_parcels_bbox(coords, srs, count=count)
    }
    else {
      catreus_bizk_wfs_get_parcels_bbox(coords2, srs, count=count)
    }
  }
  else if ((province1 == "Gipuzkoa") & (province2 == "Gipuzkoa")){
    print("Province of Gipuzkoa:")
    print("-------------------------------")
    if (srs != 25830){
      srs = 25830
    }
    if (is.null(count)){
      count <- min_count
    }
    if (is.null(coords2)){
      catreus_gipu_wfs_get_parcels_bbox(coords, srs, count=count)
    }
    else {
      catreus_gipu_wfs_get_parcels_bbox(coords2, srs, count=count)
    }
  }
  else if ((province1 == "Araba/\u00C1lava") & (province2 =="Araba/\u00C1lava")){
    print("Province of Araba/\u00C1lava:")
    print("-------------------------------")
    if (is.null(count)){
      count <- min_count
    }
    if (srs != 25830){
      srs = 25830
    }
    if (is.null(coords2)){
      crs = 4326
      coords_prov <- matrix(coords, ncol = 2, byrow = TRUE)
      sf_object <- st_as_sf(data.frame(x = coords_prov[, 2], y = coords_prov[, 1]), coords = c("x", "y"), crs = crs)
      transformed_sf_25830 <- st_transform(sf_object, crs = 25830)
      coords_25830 <- st_coordinates(transformed_sf_25830)
      lat1 = coords_25830[1, "X"]
      long1 = coords_25830[1, "Y"]
      lat2 = coords_25830[2, "X"]
      long2 = coords_25830[2, "Y"]
      coords3 = c(lat1,long1,lat2,long2)
      catreus_arab_wfs_get_parcels_bbox(coords3, srs, count=count, verbose = verbose)
    }
    else{
      catreus_arab_wfs_get_parcels_bbox(coords, srs, count=count, verbose = verbose)
    }
  }
  else if (province1 != province2){
    stop("This coordinates englobe 2 different province. ", province1 ," and ", province2,". Please select coordinates from 1 province.")
  }
  else {
    stop("This coordinates doesn´t take regions on the Basque Country. 
            Change them for ones inside the Basque Country Region")
  }
}
