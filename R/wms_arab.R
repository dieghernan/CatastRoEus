#' WMS INSPIRE: Download map images from the Álava region
#'
#' @importFrom utils modifyList unzip
#' @importFrom sf st_polygon st_sfc st_sf st_crs<-
#'
#' @description
#' Retrieve geotagged images from the Álava Cadastre. This function configures a WMS service provider
#' with [mapSpain::esp_make_provider()] and fetches tiles using a custom function `getTiles_eus` (from `utils_wms.R`).
#'
#' @inheritParams catreus_bizk_atom_get_buildings
#' @inheritParams catreus_arab_wfs_get_buildings_bbox
#' @param x A numeric vector of bbox coordinates or an `sf` object defining the area to be covered.
#' @param srs The spatial reference system to be used, typically an EPSG code.
#' @param what Specifies the layer to be extracted, see **Layers** for more details.
#' @param styles Specifies the style of the WMS layer, see **Styles** for options.
#' @param id An identifier for the custom WMS service configuration.
#' @param update_cache Logical; controls whether to refresh cached data.
#' @param cache_dir Character; directory to store cached data.
#' @param verbose Logical; controls output verbosity.
#' @param crop Logical; determines if the returned image should be cropped to the bbox.
#' @param options List; additional options for the WMS request.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return
#' Returns a [`SpatRaster`][terra::rast] object with either RGB or RGBA layers depending on the data.
#'
#' @family INSPIRE
#' @family WMS
#' @family spatial data layers from Álava
#'
#' @seealso
#' [mapSpain::esp_getTiles()], [mapSpain::esp_make_provider()], [terra::RGB()].
#' For advanced plotting, see [terra::plotRGB()] and [tidyterra::geom_spatraster_rgb()].
#'
#' @details
#' The function facilitates the selection of specific layers from the WMS service of Álava Cadastre:
#'
#' # Layers
#' Available layers include:
#' - `"parcel"`: CP.CadastralParcel
#' - `"admunit"`: AU.AdministrativeUnit
#' - `"admbound"`: AU.AdministrativeBoundary
#' - `"building"`: BU.Building
#'
#' # Styles
#' Various styles are available for each layer, which can enhance the visualization:
#' - `"parcel"`: `"INSPIRE_CP:CP.CadastralParcel.Default"`, `"INSPIRE_CP:CP.CadastralParcel.ELFCadastre"`, `"INSPIRE_CP:CP.CadastralParcel.BoundariesOnly"`, `"INSPIRE_CP:CP.CadastralParcel.LabelOnReferencePoint"`, `"INSPIRE_CP:CP.CadastralParcel.ReferencePointOnly"`
#' - `"admunit"`: `"INSPIRE_AU:AU.AdministrativeUnit.Default"`
#' - `"admbound"`: `"INSPIRE_AU:AU.AdministrativeBoundary.Default"`
#' - `"building"`: `"INSPIRE_BU:BU.Building.Default"`
#'
#' @examples
#' \donttest{
#'
#' library(mapSpain)
#' library(ggplot2)
#' library(terra)
#'
#' # Retrieving a parcel layer using specific bounding box coordinates
#' pic_pa <- catreus_arab_wms_get_layer(c(-300041.611248, 5288926.407393, -299339.346050, 5289284.705963),
#'   srs = 3857, what = "parcel", id = "layer_pa_1"
#' )
#'
#' # Displaying the fetched raster image
#' ggplot() +
#'   geom_spatraster_rgb(data = pic_pa)
#'
#' # Retrieving an administrative unit layer using specific bounding box coordinates
#' pic_au <- catreus_arab_wms_get_layer(c(-321035.518798, 5283327.395071, -284345.745221, 5300449.289407),
#'   srs = 3857, what = "admunit", id = "layer_au_1"
#' )
#'
#' # Displaying the fetched raster image
#' ggplot() +
#'   geom_spatraster_rgb(data = pic_au)
#'
#' # Retrieving an administrative boundary layer using specific bounding box coordinates
#' pic_ab <- catreus_arab_wms_get_layer(c(-321035.518798, 5283327.395071, -284345.745221, 5300449.289407),
#'   srs = 3857, what = "admbound", id = "layer_ab_1"
#' )
#'
#' # Displaying the fetched raster image
#' ggplot() +
#'   geom_spatraster_rgb(data = pic_ab)
#'
#' # Retrieving a building layer using specific bounding box coordinates
#' pic_bu <- catreus_arab_wms_get_layer(c(-298730.238481, 5288011.551711, -296360.690604, 5289922.477418),
#'   srs = 3857, what = "building", id = "layer_bu_1"
#' )
#'
#' # Displaying the fetched raster image
#' ggplot() +
#'   geom_spatraster_rgb(data = pic_bu)
#' }
#'
#' @noRd

catreus_arab_wms_get_layer <- function(x,
                                       srs,
                                       what = c("parcel", "admunit", "admbound", "building"),
                                       styles = NULL,
                                       id,
                                       update_cache = FALSE,
                                       cache_dir = NULL,
                                       verbose = FALSE,
                                       crop = FALSE,
                                       options = NULL,
                                       ...) {
  xmin <- x[1]
  ymin <- x[2]
  xmax <- x[3]
  ymax <- x[4]

  coords <- matrix(
    c(
      xmin, ymin, # bottom-left
      xmax, ymin, # bottom-right
      xmax, ymax, # top-right
      xmin, ymax, # top-left
      xmin, ymin
    ), # close polygon (repeat the first point)
    ncol = 2, byrow = TRUE
  )
  bbox_res <- get_sf_from_bbox(x, srs)
  polygon <- st_polygon(list(coords))
  sfc <- st_sfc(polygon)
  sf_object <- st_sf(geometry = sfc)
  st_crs(sf_object) <- 3857
  cache_dir <- catreus_hlp_cachedir(cache_dir)

  # Manage layer
  what <- match.arg(what)

  layer <- switch(what,
    "parcel" = "CP.CadastralParcel",
    "admunit" = "AU.AdministrativeUnit",
    "admbound" = "AU.AdministrativeBoundary",
    "building" = "BU.Building"
  )

  # Manage styles and options
  # Custom options
  opts <- list(
    styles = styles,
    version = "1.3.0"
  )

  # Add srs
  if (!missing(srs)) {
    if (!any(grepl("epsg", srs, ignore.case = TRUE))) {
      opts <- modifyList(
        opts,
        list(srs = paste0("EPSG:", srs))
      )
    }
  }

  # Add to options
  if (is.null(options)) {
    finalopts <- opts
  } else {
    names(options) <- tolower(names(options))
    finalopts <- modifyList(
      opts,
      options
    )
  }

  # Check if need to change crs
  if (finalopts$version >= "1.3.0") {
    newnames <- gsub("srs", "crs", names(finalopts))
    names(finalopts) <- newnames
  }

  # Query
  if (layer == "CP.CadastralParcel") {
    query <- "https://geo.araba.eus/WMS_INSPIRE_CP?"
    if (is.null(styles)) {
      custom_wms <- mapSpain::esp_make_provider(
        id = id,
        q = query,
        request = "GetMap",
        service = "WMS",
        version = "1.3.0",
        format = "image/png",
        layers = layer,
        crs = "EPSG:3857",
        bbox = x,
        styles = "INSPIRE_CP:CP.CadastralParcel.Default",
        width = "512",
        height = "512"
      )
    } else {
      custom_wms <- mapSpain::esp_make_provider(
        id = id,
        q = query,
        request = "GetMap",
        service = "WMS",
        version = "1.3.0",
        format = "image/png",
        layers = layer,
        crs = "EPSG:3857",
        bbox = x,
        styles = styles,
        width = "512",
        height = "512"
      )
    }
  } else if (layer == "BU.Building") {
    query <- "https://geo.araba.eus/WMS_INSPIRE_BU?"
    custom_wms <- mapSpain::esp_make_provider(
      id = id,
      q = query,
      request = "GetMap",
      service = "WMS",
      version = "1.3.0",
      format = "image/png",
      layers = layer,
      crs = "EPSG:3857",
      bbox = x,
      styles = "INSPIRE_BU:BU.Building.Default",
      width = "512",
      height = "512"
    )
  } else {
    query <- "https://geo.araba.eus/WMS_INSPIRE_AU?"
    if (layer == "AU.AdministrativeUnit") {
      style <- "INSPIRE_AU:AU.AdministrativeUnit.Default"
    } else {
      style <- "INSPIRE_AU:AU.AdministrativeBoundary.Defaul"
    }
    custom_wms <- mapSpain::esp_make_provider(
      id = id,
      q = query,
      request = "GetMap",
      service = "WMS",
      version = "1.3.0",
      format = "image/png",
      layer = layer,
      crs = "EPSG:3857",
      bbox = x,
      style = style,
      width = "512",
      height = "512"
    )
  }
  out <- getTiles_eus(sf_object, custom_wms)

  if (crop) {
    out <- terra::crop(out, bbox_res)
  }

  return(out)
}
