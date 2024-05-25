#' ATOM INSPIRE: Reference database for ATOM cadastral parcels
#'
#' @description
#' Create a database containing the urls provided in the INSPIRE ATOM service
#' for extracting Cadastral Parcels.
#'
#' @source
#' [INSPIREBIZKAIA](https://www.bizkaia.eus/es/inspirebizkaia)
#'
#' @family ATOM
#' @family parcels
#'
#' @inheritParams catreus_set_cache_dir
#'
#' @param cache A logical whether to do caching. Default is `TRUE`. See
#'   **About caching** section on [catreus_set_cache_dir()].
#'
#' @param update_cache A logical whether to update cache. Default is `FALSE`.
#'  When set to `TRUE` it would force a fresh download of the source file.
#'
#' @rdname catreus_bizk_atom_get_parcels_db
#' @export
#'
#' @return
#' A [tibble][tibble::tibble] with the information requested:
#'   - `munic`: Name of the municipality.
#'   - `url`: url for downloading information of the corresponding municipality.
#'   - `date`: Reference date of the data.
#'
#' @examples
#' \donttest{
#' catreus_bizk_atom_get_parcels_db_all()
#' }

catreus_bizk_atom_get_parcels_db_all <- function(cache = TRUE,
                                                 update_cache = FALSE,
                                                 cache_dir = NULL,
                                                 verbose = FALSE) {
  api_entry <- paste0(
    "https://apli.bizkaia.eus/",
    "apps/Danok/INSPIRE/",
    "cadastralparcels.xml"
  )

  filename <- basename(api_entry)

  path <- catreus_hlp_dwnload(
    api_entry, filename, cache_dir,
    verbose, update_cache, cache
  )

  tbl <- catreus_read_atom(path)
  names(tbl) <- c("munic", "url", "date")
  return(tbl)
}

#' ATOM INSPIRE: Download all the cadastral parcels of a municipality
#'
#' @description
#' Get the spatial data of all the cadastral parcels belonging to a single
#' municipality using the INSPIRE ATOM service.
#'
#' @references
#' [INSPIREBIZKAIA](https://www.bizkaia.eus/es/inspirebizkaia)
#'
#' @family ATOM
#' @family parcels
#'
#' @export
#' @return A [`sf`][sf::st_sf] object.
#'
#' @inheritParams catreus_bizk_atom_get_parcels_db_all
#' @param munic Municipality to extract, It can be a part of a string or the
#'   cadastral code. See [catreus_bizk_atom_get_parcels_db_all].
#' @examples
#' \donttest{
#'
#' s <- catreus_bizk_atom_get_parcels("ABANTO Y CIERVANA")
#'
#' library(ggplot2)
#'
#' ggplot(s) +
#'   geom_sf() +
#'   labs(
#'     title = "Cadastral Zoning",
#'     subtitle = "ABANTO Y CIERVANA"
#'   )
#' }


catreus_bizk_atom_get_parcels <- function(munic,
                                          cache = TRUE,
                                          update_cache = FALSE,
                                          cache_dir = NULL,
                                          verbose = FALSE) {
  all <- catreus_bizk_atom_get_parcels_db_all(
    cache = cache,
    update_cache = update_cache,
    cache_dir = cache_dir,
    verbose = FALSE
  )

  findmunic <- grep(munic, all$munic, ignore.case = TRUE)[1]

  if (is.na(findmunic)) {
    message(
      "No Municipality found for ", munic,
      ". Check available municipalities with catreus_atom_get_parcels_db_all()"
    )
    return(invisible(NA))
  }
  m <- all[findmunic, ]

  if (verbose) {
    message("Selecting ", m$munic)
  }

  # Download from url
  api_entry <- m$url[1]

  filename <- basename(api_entry)


  path <- catreus_hlp_dwnload(
    api_entry, filename, cache_dir,
    verbose, update_cache, cache
  )

  # To a new directory
  # Get cached dir
  cache_dir <- catreus_hlp_cachedir(cache_dir)

  unlist(strsplit(filename, ".", fixed = TRUE))[1]

  exdir <- file.path(
    cache_dir,
    unlist(strsplit(filename, ".", fixed = TRUE))[1]
  )

  if (!dir.exists(exdir)) dir.create(exdir, recursive = TRUE)
  unzip(path, exdir = exdir, junkpaths = TRUE, overwrite = TRUE)


  # Guess what to read
  files <- list.files(exdir, full.names = TRUE, pattern = ".gml$")
  print(files)
  print(length(files))
  if (length(files) > 1) {
    files = (files[3])
  } else {
    files = files
  }
  print(files)
  print(length(files))

  sfobj <- st_read_layers_encoding(files, verbose)

  return(sfobj)
}
