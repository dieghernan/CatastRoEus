#' WMS INSPIRE: Download map images from the Bizkaia region
#' 
#' @importFrom utils modifyList unzip
#' @importFrom sf st_polygon st_sfc st_sf st_crs<-
#'
#' @description
#' Retrieve geotagged images from the Bizkaia Cadastre. This function configures a WMS service provider
#' with [mapSpain::esp_make_provider()] and fetches tiles using a custom function `getTiles_eus` (from `utils_wms.R`).
#'
#' @inheritParams catreus_bizk_atom_get_buildings
#' @inheritParams catreus_bizk_wfs_get_buildings_bbox
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
#' @family spatial data layers from Bizkaia
#'
#' @seealso
#' [mapSpain::esp_getTiles()], [mapSpain::esp_make_provider()], [terra::RGB()]. 
#' For advanced plotting, see [terra::plotRGB()] and [tidyterra::geom_spatraster_rgb()].
#'
#' @details
#' The function facilitates the selection of specific layers from the WMS service of Bizkaia Cadastre:
#'
#' # Layers
#' Available layers include:
#' - `"parcel"`: CP.CadastralParcel
#' - `"zoning"`: CP.CadastralZoning
#' - `"address"`: AD.Address
#' - `"admunit"`: AU.AdministrativeUnit
#'
#' # Styles
#' Various styles are available for each layer, which can enhance the visualization:
#' - `"parcel"`: `"CP.CadastralParcel.BoundariesOnly"`, `"CP.CadastralParcel.ReferencePointOnly"`, `"CP.CadastralParcel.LabelOnReferencePoint"`, `"CP.CadastralParcel.Default"`, `"inspire_common:DEFAULT"`
#' - `"zoning"`: `"CP.CadastralZoning.Default"`, `"inspire_common:DEFAULT"`
#' - `"address"`: `"AD.Address.Default"`, `"inspire_common:DEFAULT"`
#' - `"admunit"`: `"AU.AdministrativeUnit.Default"`, `"inspire_common:DEFAULT"`
#'
#' @examples
#' \donttest{
#' 
#' library(mapSpain)
#' library(ggplot2)
#' library(terra)
#' 
#' # Retrieving a parcel layer using specific bounding box coordinates
#' pic_pa=catreus_bizk_wms_get_layer(c(-336552.23553989833, 5359836.083070541, -335018.7176594635, 5361383.932893863), 
#'                           srs= 3857, what = "parcel", id = "layer_pa_1")
#'
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_pa)
#' 
#' # Retrieving a zoning layer using specific bounding box coordinates
#' pic_zo=catreus_bizk_wms_get_layer(c(-339724.372213,5356554.068169,-333915.158064,5361598.912035), 
#'                           srs= 3857, what = "zoning", id = "layer_zo_1")
#'
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_zo)
#' 
#' # Retrieving an address layer using specific bounding box coordinates
#' pic_ad=catreus_bizk_wms_get_layer(c(-336552.23553989833, 5359836.083070541, -335018.7176594635, 5361383.932893863), 
#'                           srs= 3857, what = "address", id = "layer_ad_1")
#'
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_ad)
#' 
#' # Retrieving an administrative unit layer using specific bounding box coordinates
#' pic_au=catreus_bizk_wms_get_layer(c(-339724.372213,5356554.068169,-333915.158064,5361598.912035), 
#'                           srs= 3857, what = "admunit", id = "layer_au_1")
#'
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_au)
#' }
#' 
#' @noRd

catreus_bizk_wms_get_layer <- function(x,
                               srs,
                               what = c("parcel", "zoning", "address", "admunit"),
                               styles = "default",
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
    c(xmin, ymin,  # bottom-left
      xmax, ymin,  # bottom-right
      xmax, ymax,  # top-right
      xmin, ymax,  # top-left
      xmin, ymin), # close polygon (repeat the first point)
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
                  "zoning" = "CP.CadastralZoning",
                  "address" = "AD.Address",
                  "admunit" = "AU.AdministrativeUnit"
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
  custom_wms <- mapSpain::esp_make_provider(id = id,
                                               q = "https://geo.bizkaia.eus/arcgisserverinspire/rest/services/Catastro/Annex1/MapServer/exts/InspireView/service?",
                                               request="GetMap",
                                               service = "WMS",
                                               version = "1.3.0",
                                               format = "image/png",
                                               layers = layer,
                                               crs="EPSG:3857",
                                               bbox=x,
                                               styles = styles,
                                               width="512",
                                               height="512")
  
  out <- getTiles_eus(sf_object, custom_wms)
  
  if (crop) {
    out <- terra::crop(out, bbox_res)
  }
  
  return(out)
}