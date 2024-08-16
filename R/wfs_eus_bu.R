#' Retrieve Building Data in Pais Vasco Based on Bounding Box Coordinates
#' 
#' @importFrom sf st_as_sf st_transform st_coordinates 
#' @importFrom sf st_buffer
#' @importFrom magrittr %>%
#' 
#' @description
#' This function fetches buildings data within a specified bounding box. It first checks if the provided coordinates are valid,
#' determines the province based on these coordinates using reverse geocoding, and fetches buildings data for
#' Bizkaia, Gipuzkoa, or Araba/Álava accordingly. It supports transformations between geographical and UTM coordinate systems,
#' handles multiple Coordinate Reference Systems (CRS), and manages different minimum counts for returned records.
#'
#' @param x Bounding box coordinates or a spatial object, which could be:
#'   - A numeric vector of length 4 with the coordinates defining the bounding box:
#'     `c(latitude1, longitude1, latitude2, longitude2)`.
#'   - An `sf/sfc` object from the \CRANpkg{sf} package.
#' @param srs Spatial Reference System (SRS) or Coordinate Reference System (CRS) code 
#'   to be used in the query. For best results, ensure the coordinates are in ETRS89
#'   (EPSG:25830).
#' @param verbose Logical; if `TRUE`, additional information about function operations 
#'   is printed. Useful for debugging. Default is `FALSE`.
#' @param count Integer specifying the maximum number of building records to return.
#'   Default is NULL.
#'
#' @return Depending on the input and geographic location, this function may return:
#'   - An `sf` object containing building data within the specified bbox.
#'   - A message indicating mismatched or out-of-region coordinates.
#'
#' @seealso \code{\link[sf:st_bbox]{st_bbox}}, which is used to manage spatial bounding boxes.
#'
#' @details
#' This function uses reverse geocoding to determine the province within the Basque Country
#' from the coordinates provided. Based on the province, it delegates the data fetching to
#' specific functions handling each province's data. It supports flexible input types and
#' handles geographical coordinate transformations internally if needed.
#'
#' @examples
#' \donttest{
#' 
#' library(mapSpain)
#' library(dplyr)
#' library(sf)
#' 
#' # Define bounding box coordinates for a location in Gipuzkoa
#' coords_gipuzkoa <- c(582181.399767, 4796308.387948, 583411.968522, 4797564.942348)
#'
#' # Fetch building data using the bounding box
#' buildings_gipuzkoa <- catreus_wfs_get_buildings_bbox(coords_gipuzkoa, srs = 25830, count = 50)
#'
#' library(ggplot2)
#' # Plot the buildings data
#' ggplot(buildings_gipuzkoa) +
#'   geom_sf() + ggtitle("Building Data for Gipuzkoa")
#'
#' # Define bounding box coordinates for a location in Bizkaia
#' bilbao <- esp_get_capimun(munic = "Bilbao") %>%
#'   st_transform(25830) %>%
#'   st_buffer(300)
#' buildings_bilbao <- catreus_wfs_get_buildings_bbox(bilbao)
#' ggplot(buildings_bilbao) + geom_sf() + ggtitle("Buildings Data for Bilbao")
#'
#' # Define bounding box coordinates for a rural location in Araba/Álava
#' coords_alaba = c(525858.205755, 4742911.412803, 526701.543389, 4743398.976145)
#' 
#' # Fetch building data using the bounding box, requesting more features
#' buildings_alaba <- catreus_wfs_get_buildings_bbox(coords_alaba, srs = 25830)
#' 
#' library(ggplot2)
#' # Plot the buildings data for Araba/Álava
#' ggplot(buildings_alaba) + geom_sf() + ggtitle("Building Data for Araba")
#' 
#' }
#'   
#' @export

catreus_wfs_get_buildings_bbox <- function(x, srs = NULL, 
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
      catreus_bizk_wfs_get_buildings_bbox(coords, srs, count=count)
    }
    else {
      catreus_bizk_wfs_get_buildings_bbox(coords2, srs, count=count)
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
      catreus_gipu_wfs_get_buildings_bbox(coords, srs, count=count)
    }
    else {
      catreus_gipu_wfs_get_buildings_bbox(coords2, srs, count=count)
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
      catreus_arab_wfs_get_buildings_bbox(coords3, srs, count=count)
    }
    else{
      catreus_arab_wfs_get_buildings_bbox(coords, srs, count=count)
    }
  }
  else if (province1 != province2) {
    stop("This coordinates englobe 2 differente province, please select coordinates for 1 province")
  } 
  else {
    stop("This coordinates doesn´t take regions on the Basque Country. 
            Change them for ones inside the Basque Country Region")
  }
}
