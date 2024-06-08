#' WMS INSPIRE: Download map images from the Gipuzkoa region
#'
#' @description
#' Retrieve geotagged images from the Gipuzkoa Cadastre. This function configures a WMS service provider
#' with [mapSpain::esp_make_provider()] and fetches tiles using a custom function `getTiles_eus` (from `utils_wms.R`).
#'
#' @inheritParams catreus_bizk_atom_get_buildings
#' @inheritParams catreus_gipu_wfs_get_buildings_bbox
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
#' @family spatial data layers from Gipuzkoa
#'
#' @seealso
#' [mapSpain::esp_getTiles()], [mapSpain::esp_make_provider()], [terra::RGB()]. 
#' For advanced plotting, see [terra::plotRGB()] and [tidyterra::geom_spatraster_rgb()].
#'
#' @details
#' The function facilitates the selection of specific layers from the WMS service of Gipuzkoa Cadastre:
#'
#' # Layers
#' Available layers include:
#' - `"parcel"`: cp.CadastralParcel
#' - `"zoning"`: cp.CadastralZoning
#' - `"address"`: ad.Address
#' - `"admunit"`: au.AdministrativeUnit
#' - `"admbound"`: au.AdministrativeBoundary
#' - `"building"`: bu.building
#' - `"buother"`: bu.otherconstruction
#'
#' # Styles
#' Various styles are available for each layer, which can enhance the visualization:
#' - `"parcel"`: `"CP.CadastralParcel.BoundariesOnly"`, `"CP.CadastralParcel.ReferencePointOnly"`, `"CP.CadastralParcel.LabelOnReferencePoint"`, `"CP.CadastralParcel.Default"`, `"CP.CadastralParcel.ELFCadastre"`
#' - `"zoning"`: `"CP.CadastralZoning.Default"`, `"CP.CadastralZoning.ELFCadastre"`
#' - `"address"`: `"AD.Address.Default"`, `"AD.Address.Number.ELFCadastre"`
#' - `"admunit"`: `"AU.AdministrativeUnit.Default"`, `"AU.AdministrativeUnit.ELFCadastre"`
#' - `"admbound"`: `"AU.AdministrativeBoundary.Default"`, `"AU.AdministrativeBoundary.ELFCadastre"`
#' - `"building"`: `"BU.Building.Default"`, `"BU.Building.ELFCadastre"`
#' - `"buother"`: `"BU.OtherConstruction.Default"`,`"BU.OtherConstruction.ELFCadastre"`
#'
#' @examples
#' \donttest{
#' 
#' library(mapSpain)
#' library(ggplot2)
#' library(terra)
#' 
#' # Retrieving a parcel layer using specific bounding box coordinates
#' pic_pa = catreus_gipu_wms_get_layer(c(-220929.286973,5360633.894553,-220654.591402,5361039.966266), 
#'                               srs= 3857, what = "parcel", id = "layer_pa_1")
#'                               
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_pa)
#' 
#' # Retrieving a zoning layer using specific bounding box coordinates
#' pic_zo = catreus_gipu_wms_get_layer(c(-220929.286973,5360633.894553,-220654.591402,5361039.966266), 
#'                               srs= 3857, what = "zoning", id = "layer_zo_1")
#'
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_zo)
#' 
#' # Retrieving an address layer using specific bounding box coordinates
#' pic_ad = catreus_gipu_wms_get_layer(c(-221868.029226,5358914.061417,-220750.137688,5360872.760267), 
#'                               srs= 3857, what = "address", id = "layer_ad_1")

#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_ad)
#' 
#' # Retrieving an administrative unit layer using specific bounding box coordinates
#' pic_au <- catreus_gipu_wms_get_layer(c(-222756.191383,5358616.194743,-219211.842493,5361751.786092), 
#'                                      srs = 3857, what = "admunit", id = "layer_au_1")
#'
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_au)
#' 
#' # Retrieving an administrative boundary layer using specific bounding box coordinates
#' pic_ab <- catreus_gipu_wms_get_layer(c(-222756.191383,5358616.194743,-219211.842493,5361751.786092), 
#'                                      srs = 3857, what = "admbound", id = "layer_ab_1")
#'
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_ab)
#' 
#' # Retrieving a building layer using specific bounding box coordinates
#' pic_bu <- catreus_gipu_wms_get_layer(c(-221868.029226,5358914.061417,-220750.137688,5360872.760267), 
#'                                      srs = 3857, what = "building", id = "layer_bu_1")
#'
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_bu)
#'
#' # Retrieving a other building layer using specific bounding box coordinates
#' pic_buo <- catreus_gipu_wms_get_layer(c(-220922.121001,5357394.875480,-220272.406261,5358111.472620), 
#'                                      srs = 3857, what = "buother", id = "layer_buo_1")
#'
#' # Displaying the fetched raster image
#' ggplot() + geom_spatraster_rgb(data = pic_buo)
#' }
#'
#' @noRd

catreus_gipu_wms_get_layer <- function(x,
                                       srs,
                                       what = c("parcel", "zoning", "address", 
                                                "admunit", "admbound", "building", "buother"),
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
  #bbox_res <- get_sf_from_bbox(x, srs)
  polygon <- st_polygon(list(coords))
  sfc <- st_sfc(polygon)
  sf_object <- st_sf(geometry = sfc)
  st_crs(sf_object) <- 3857
  cache_dir <- catreus_hlp_cachedir(cache_dir)
  
  # Manage layer
  what <- match.arg(what)
  
  layer <- switch(what,
                  "parcel" = "cp.CadastralParcel",
                  "zoning" = "cp.CadastralZoning",
                  "address" = "ad.Address",
                  "admunit" = "au.AdministrativeUnit",
                  "admbound" = "au.AdministrativeBoundary",
                  "building" = "bu.building",
                  "buother" = "bu.otherconstruction"
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
                                            q = "https://b5m.gipuzkoa.eus/inspire/wms/gipuzkoa_wms?",
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
