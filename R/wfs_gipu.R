#' Download addresses of Gipuzkoa in spatial format
#'
#' @description
#' Get the spatial data of addresses by bounding box.
#'
#' @inheritParams catreus_gipu_wfs_get_address_bbox
#'
#' @seealso [sf::st_bbox()]
#'
#' @return A [`sf`][sf::st_sf] object.
#' @source
#' [Gipuzkoa Spatial Data Infrastructure](https://b5m.gipuzkoa.eus/web5000/en/inspire-services)
#'
#'
#' @examples
#' \donttest{
#'
#' x <- c(43.026899, -2.433164, 43.308497, -2.063099)
#' ad <- catreus_gipu_wfs_get_address_bbox(x, srs=25830,  count=10)
#'
#' library(ggplot2)
#'
#' ggplot(ad) +
#'   geom_sf()
#' }
#'
#' @seealso [CatastRo::catr_wfs_get_address_bbox()]
#' @seealso [CatastRoNav::catrnav_wfs_get_address_bbox()]
#' @noRd

catreus_gipu_wfs_get_address_bbox <- function(x, srs, verbose = FALSE,
                                               count = NULL) {
  # Switch to stored queries
  stored_query <- "AD:Address"

  bbox_res <- wfs_bbox(x, srs)
  res <- wfs_api_query(
    host = "https://b5m.gipuzkoa.eus/inspire/",
    entry = "wfs/gipuzkoa_wfs_ad?",
    verbose = verbose,
    # WFS service
    version = "2.0.0",
    service = "WFS",
    request = "getfeature",
    typenames = stored_query,
    count = count,
    # Stored query
    bbox = bbox_res$bbox,
    SRS = bbox_res$incrs
  )
  out <- wfs_results(res, verbose)

  if (!is.null(out)) {
    if (is.na(sf::st_crs(out))) {
      # Asigna el CRS de 'bbox_res' a 'out' si no está definido
      sf::st_crs(out) <- sf::st_crs(bbox_res$outcrs)
    }
    # Transform back to the desired srs
    out <- sf::st_transform(out, bbox_res$outcrs)
  }
  return(out)
}

#' Download buildings of Gipuzkoa in spatial format
#'
#' @description
#' Get the spatial data of buildings by bounding box.
#'
#' @param x See **Details**. It could be:
#'   - A numeric vector of length 4 with the coordinates that defines
#'     the bounding box: `c(xmin, ymin, xmax, ymax)`.
#'   - A `sf/sfc` object, as provided by the \CRANpkg{sf} package.
#' @param srs SRS/CRS to use on the query. See **Details**.
#' @param verbose Logical, displays information. Useful for debugging, default
#'   is `FALSE`.
#' @param count integer, indicating the maximum number of features to return.
#'   The default value `NULL` does not pass this parameter to the query,
#'   and the maximum number of features would be determined by the default value
#'   of the API service (5,000 in this case).
#'
#' @seealso [sf::st_bbox()]
#'
#' @return A [`sf`][sf::st_sf] object.
#' @source
#' [Gipuzkoa Spatial Data Infrastructure](https://b5m.gipuzkoa.eus/web5000/en/inspire-services)
#'
#'
#' @examples
#' \donttest{
#' x <- c(42.994337, -2.554863, 43.344138, -1.779831)
#' bu <- catreus_gipu_wfs_get_parcels_bbox(x, srs=25830,  count=10)
#'
#' library(ggplot2)
#'
#' ggplot(bu) +
#'   geom_sf()
#' }
#'
#' @seealso [CatastRo::catr_wfs_get_buildings_bbox()]
#' @seealso [CatastRoNav::catrnav_wfs_get_buildings_bbox()]
#' @noRd

catreus_gipu_wfs_get_buildings_bbox <- function(x, srs, verbose = FALSE,
                                                count = NULL) {
  # Switch to stored queries
  stored_query <- "bu-ext2d:Building"

  bbox_res <- wfs_bbox(x, srs)
  res <- wfs_api_query(host= "https://b5m.gipuzkoa.eus/inspire/",
     entry = "wfs/gipuzkoa_wfs_bu?",
     verbose = verbose,
     # WFS service
     version = "2.0.0",
     service = "WFS",
     request = "getfeature",
     typenames = stored_query,
     count = count,
     # Stored query
     bbox = bbox_res$bbox,
     SRS = bbox_res$incrs
  )

  out <- wfs_results(res, verbose)

  if (!is.null(out)) {
    if (is.na(sf::st_crs(out))) {
      # Asigna el CRS de 'bbox_res' a 'out' si no está definido
      sf::st_crs(out) <- sf::st_crs(bbox_res$outcrs)
    }
    # Transform back to the desired srs
    out <- sf::st_transform(out, bbox_res$outcrs)
  }
  return(out)
}

#' Download cadastral parcels of Gipuzkoa in spatial format
#'
#' @description
#' Get the spatial data of cadastral parcels by bounding box.
#'
#' @inheritParams catreus_bizk_wfs_get_buildings_bbox
#'
#' @seealso [sf::st_bbox()]
#'
#' @family parcels
#'
#' @return A [`sf`][sf::st_sf] object.
#' @source
#' [Gipuzkoa Spatial Data Infrastructure](https://b5m.gipuzkoa.eus/web5000/en/inspire-services)
#'
#'
#' @examples
#' \donttest{
#' x <- c(43.312869, -1.929140, 43.312839, -1.929380)
#' cp <- catreus_gipu_wfs_get_parcels_bbox(x, srs=25830,  count=10)
#'
#' library(ggplot2)
#'
#' ggplot(cp) +
#'   geom_sf()
#' }
#'
#' @seealso [CatastRo::catr_wfs_get_parcels_bbox()]
#' @seealso [CatastRoNav::catrnav_wfs_get_parcels_bbox()]
#' @noRd

catreus_gipu_wfs_get_parcels_bbox <- function(x, srs, verbose = FALSE,
                                              count = NULL) {
  # Switch to stored queries
  stored_query <- "cp:CadastralParcel"

  bbox_res <- wfs_bbox(x, srs)

  res <- wfs_api_query(
    host = "https://b5m.gipuzkoa.eus/inspire/",
    entry = "wfs/gipuzkoa_wfs_cp?",
    verbose = verbose,
    # WFS service
    version = "2.0.0",
    service = "WFS",
    request = "getfeature",
    typenames = stored_query,
    count = count,
    # Stored query
    bbox = bbox_res$bbox,
    SRS = bbox_res$incrs
  )

  out <- wfs_results(res, verbose)

  if (!is.null(out)) {
    if (is.na(sf::st_crs(out))) {
      # Asigna el CRS de 'bbox_res' a 'out' si no está definido
      sf::st_crs(out) <- sf::st_crs(bbox_res$outcrs)
    }
    # Transform back to the desired srs
    out <- sf::st_transform(out, bbox_res$outcrs)
  }
  return(out)
}
