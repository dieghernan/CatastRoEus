#' Get full Url
#'
#' It is used to build a complete URL to access a specific WFS service.
#' @param host Main part of the URL
#' @param entry Specific part of the URL that identifies the resource you want
#' to access on the server
#' @param params Query parameters to be appended to the URL
#'
#' @returns Complete URL
#' @noRd


wfs_get_url <- function(host,entry,params){
  #Clean empty params
  params <- params[length(params) != 0]

  #Create query string
  query <- paste0(names(params), "=", params, collapse = "&")

  #Create full url
  url <- paste0(host, entry, query)
  return(url)
}

#' Check if an archive specified by it's path contains GML element
#'
#' @param path path of the archive we want to check
#'
#' @returns TRUE if GML elements are contained FALSE if not
#' @noRd


wfs_check <- function(path){
  #Check if it contains gml
  lines <- suppressWarnings(readLines(path,n=20))

  if (any(grepl("<gml", lines))){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Handle WFS queries
#'
#' Prepare a WFS query and download the data in GML format
#'
#' @param entry The specific part of the URL that identifies the resource to be
#' accessed on the server.
#' @param ... Query parameters
#' @param verbose Whether to display additional info on call
#'
#' @returns A list indicating the path to the tempfile, if it is a sf
#' object and in case of error a message to display
#'
#' @noRd


wfs_api_query <- function(host, entry, ..., verbose = TRUE) {
  arguments <- list(...)

  # Handle SRS. Is optional but if provided check
  srs <- arguments$srsName

  if (!is.null(srs)) {
    arguments$srsName <- paste0("EPSG:", srs)
  }

  # Prepare URL
  # Get URl
  api_entry <- wfs_get_url(host = host,
    entry = entry,
    params = arguments
  )

  # Filename
  filename <- paste0(basename(tempfile()), ".gml")

  path <- catreus_hlp_dwnload(
    api_entry, filename,
    cache_dir = tempdir(),
    verbose = verbose, update_cache = FALSE,
    cache = TRUE
  )

  # Check
  is_sf <- wfs_check(path)

  # Prepare outlist
  outlist <- list(
    is_sf = is_sf,
    path = path
  )

  if (!is_sf) {
    m <- unlist(xml2::as_list(xml2::read_xml(path)))

    outlist$m <- m
  }
  return(outlist)
}


#' Verifies whether the results are valid and returns the corresponding spatial data if the query was successful.
#'
#' @param res Object containing the results of the query to the WFS service
#' @param verbose Logical value indicating whether detailed messages should be displayed during the execution of the function.
#'
#' @returns The spatial data obtained from the query to the WFS service if the results are valid and the query was successful
#' @noRd


wfs_results <- function(res, verbose) {
  # Check result
  if (res$is_sf) {
    out <- st_read_layers_encoding(res$path, verbose)
    out <- sf::st_make_valid(out)

    unlink(res$path, force = TRUE)
    return(out)
  } else {
    message("Malformed query: ", res$m)
    unlink(res$path, force = TRUE)
    return(invisible(NULL))
  }
}


#' Create a spatial object (sf or sfc) from a bounding box (bbox) and a specified spatial reference system (srs)
#'
#' @param bbox Bounding box
#' @param srs Spatial reference system of the bbox
#'
#' @returns Spatial object (sf or sfc) representing the bounding box with the specified spatial reference system.
#' @noRd


get_sf_from_bbox <- function(bbox, srs) {
  if (inherits(bbox, "sf") || inherits(bbox, "sfc")) {
    return(bbox)
  }

  # Sanity check
  if (!(is.numeric(bbox) && length(bbox) == 4)) {
    stop("bbox should be a vector of 4 numbers.", call. = FALSE)
  }

  if (missing(srs)) stop("Please provide a srs value", call. = FALSE)

  # Create template for a spatial bbox
  template_sf <- sf::st_sfc(sf::st_point(c(0, 0)))
  template_bbox <- sf::st_bbox(template_sf)

  # Create the spatial object
  bbox_new <- bbox
  class(bbox_new) <- class(template_bbox)

  bbox_new <- sf::st_as_sfc(bbox_new)
  bbox_new <- sf::st_set_crs(bbox_new, srs)

  return(bbox_new)
}


#' Takes a bounding box and a spatial reference system (SRS) as input and returns an object with information about the transformed
#' bounding box and spatial reference systems.
#'
#' @param bbox Bounding box
#' @param srs Spatial reference system
#'
#' @returns List object with bbox (character string representing the values of the transformed bounding box, separated by commas),
#' outcrs (spatial reference system of the input bounding box), incrs (spatial reference system usedfor transforming the bounding box
#' to a specific spatial reference system (in this case, 25830))
#' @noRd


wfs_bbox <- function(bbox, srs) {
  result <- list()

  # Use bbox of a spatial object. The API fails on geografic coord ¿?
  if (inherits(bbox, "sf") || inherits(bbox, "sfc")) {
    # Convert to 25830 (opinionated)
    bbox_new <- sf::st_transform(bbox, 25830)

    # Get bbox values
    values <- as.double(sf::st_bbox(bbox_new))

    result$bbox <- paste0(values, collapse = ",")
    result$outcrs <- sf::st_crs(bbox)
    result$incrs <- 25830
  } else {
    # Convert to sf
    bbox_new <- get_sf_from_bbox(bbox, srs)
    result$outcrs <- sf::st_crs(bbox_new)
    bbox_new <- sf::st_transform(bbox_new, 25830)
    result$incrs <- 25830

    # Get bbox values
    values <- as.double(sf::st_bbox(bbox_new))

    result$bbox <- paste0(values, collapse = ",")
  }

  return(result)
}
