#' Get static tiles from public administrations of Spain
#'
#' @description
#' Get static map tiles based on a spatial object. Maps can be fetched from
#' various open map servers.
#'
#' @importFrom utils modifyList
#' @noRd

getTiles_eus <- function(x,
                         type = "IDErioja",
                         zoom = NULL,
                         zoommin = 0,
                         crop = TRUE,
                         res = 512,
                         bbox_expand = 0.05,
                         transparent = TRUE,
                         mask = FALSE,
                         update_cache = FALSE,
                         cache_dir = NULL,
                         verbose = FALSE,
                         options = NULL) {
  if (!requireNamespace("slippymath", quietly = TRUE)) {
    stop("slippymath package required for using this function")
  }
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("terra package required for using this function")
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("png package required for using this function")
  }
  # nocov end
  # Only sf and sfc objects allowed

  if (!inherits(x, "sf") && !inherits(x, "sfc")) {
    stop(
      "Only sf and sfc ",
      "objects allowed"
    )
  }

  # If sfc convert to sf
  if (inherits(x, "sfc")) {
    x <- sf::st_as_sf(
      data.frame(x = 1),
      x
    )
  }
  # Some transformations
  res <- as.numeric(res)
  # Keep initial
  xinit <- x
  x <- sf::st_geometry(x)
  # A. Check providers
  if (is.list(type)) {
    # Custom query
    url_pieces <- type$q
    type <- type$id
    if (any(is.null(url_pieces), is.null(type))) {
      stop(
        "Custom provider should be a named list with an 'id' ",
        "and a 'q' field"
      )
    }
    url_pieces <- eus_hlp_split_url(url_pieces)
    extra_opts <- NULL
  } else {
    provs <- mapSpain::esp_tiles_providers
    if (!type %in% names(provs)) {
      stop(
        "No match for type = '",
        type,
        "' found. Check providers available in mapSpain::esp_tiles_providers"
      )
    }
    # Split url
    url_pieces <- provs[[type]]$static
    # And get extra optios
    extra_opts <- provs[[type]]$leaflet

    names(url_pieces) <- tolower(names(url_pieces))
    names(extra_opts) <- tolower(names(extra_opts))
  }
  # Create cache dir
  cache_dir <- catreus_hlp_cachedir(cache_dir)
  cache_dir <- catreus_hlp_cachedir(paste0(cache_dir, "/", type))


  # Attribution
  attr <- url_pieces$attribution

  url_pieces <- modifyList(url_pieces, list(attribution = NULL))

  # Get type of service
  if (is.null(url_pieces$service)) {
    # On null we assume WMTS, case of non INSPIRE serves OSM)
    typeprov <- "WMTS"
  } else {
    typeprov <- toupper(url_pieces$service)
  }
  # Add options
  if (is.list(options)) {
    names(options) <- tolower(names(options))

    if (typeprov == "WMS" && "version" %in% names(options)) {
      # Exception: need to change names depending on the version of WMS
      v_wms <- unlist(modifyList(
        list(v = url_pieces$version),
        list(v = options$version)
      ))

      # Assess version
      v_wms <- unlist(strsplit(v_wms, ".", fixed = TRUE))

      if (v_wms[1] >= "1" && v_wms[2] >= "3") {
        names(url_pieces) <- gsub("srs", "crs", names(url_pieces))
      } else {
        names(url_pieces) <- gsub("crs", "srs", names(url_pieces))
      }
    }

    # Ignore TileMatrix fields in WMTS
    if (typeprov == "WMTS") {
      ig <- !grepl("tilematrix", names(options), ignore.case = TRUE)
      options <- options[ig]
    }

    url_pieces <- modifyList(url_pieces, options)
    # Create new cache dir

    # Modify cache dir
    newdir <- paste0(names(options), "=", options, collapse = "&")
    newdir <- eus_get_md5(newdir)

    cache_dir <- file.path(cache_dir, newdir)
    cache_dir <- catreus_hlp_cachedir(cache_dir)
  }

  # Get CRS of Tile
  crs <- unlist(
    url_pieces[names(url_pieces) %in% c("crs", "srs", "tilematrixset")]
  )
  # Caso some WMTS
  if (is.null(crs)) crs <- "epsg:3857"

  if (tolower(crs) == tolower("GoogleMapsCompatible")) crs <- "epsg:3857"

  crs_sf <- sf::st_crs(crs)
  # Transform to crs of tile
  x <- sf::st_transform(x, crs_sf)

  # Buffer if single point
  if (length(x) == 1 && "POINT" %in% sf::st_geometry_type(x)) {
    xmod <- sf::st_transform(sf::st_geometry(x), 3857)
    xmod <- sf::st_buffer(xmod, 50)
    x <- sf::st_transform(xmod, sf::st_crs(x))
    crop <- FALSE
    # Auto zoom = 15 if not set
    if (is.null(zoom)) {
      zoom <- 15
      if (verbose) message("Auto zoom on point set to 15")
    }
  }
  newbbox <- eus_hlp_get_bbox(x, bbox_expand, typeprov)

  if (typeprov == "WMS") {
    rout <-
      getwms(
        newbbox,
        url_pieces,
        update_cache,
        cache_dir,
        verbose,
        res,
        transparent
      )
  } else {
    rout <-
      getwmts(
        newbbox,
        type,
        url_pieces,
        update_cache,
        cache_dir,
        verbose,
        zoom,
        zoommin,
        transparent,
        extra_opts
      )
  }
  # Regenerate
  # Display attributions

  if (verbose && !is.null(attr)) {
    message(
      "\nData and map tiles sources:\n", attr
    )
  }
  x <- xinit
  x_terra <- terra::vect(x)

  # reproject rout if needed
  if (!sf::st_crs(x) == sf::st_crs(rout)) {
    # Sometimes it gets an error

    rout_end <- try(terra::project(
      rout,
      terra::crs(x_terra)
    ), silent = TRUE)

    if (inherits(rout_end, "try-error")) {
      if (verbose) message("Tile not reprojected.")
      rout <- rout
    } else {
      rout <- rout_end
    }
  }
  rout <- terra::clamp(rout,
    lower = 0,
    upper = 255,
    values = TRUE
  )
  # crop management
  if (crop == TRUE) {
    newbbox <- sf::st_transform(newbbox, sf::st_crs(x))
    cb <- sf::st_bbox(newbbox)

    rout <- terra::crop(rout, cb[c(1, 3, 2, 4)])
  }

  # Mask
  if (mask) {
    rout <- terra::mask(rout, x_terra)
  }

  # Manage transparency

  if (!transparent && terra::nlyr(rout) == 4) {
    rout <- terra::subset(rout, 1:3)
  }

  # Manage RGB
  if (isFALSE(terra::RGB(rout))) {
    terra::RGB(rout) <- seq_len(terra::nlyr(rout))
  }


  # Result
  return(rout)
}


#' @name eus_hlp_split_url
#' @importFrom utils modifyList
#' @noRd

eus_hlp_split_url <- function(url_static) {
  split <- unlist(strsplit(url_static, "?", fixed = TRUE))

  if (length(split) == 1) {
    return(list(q = split))
  }

  urlsplit <- list()
  urlsplit$q <- paste0(split[1], "?")

  opts <- unlist(strsplit(split[2], "&"))

  names_opts <- vapply(opts, function(x) {
    n <- strsplit(x, "=", fixed = TRUE)
    return(unlist(n)[1])
  }, FUN.VALUE = character(1))

  values_opts <- vapply(opts, function(x) {
    n <- strsplit(x, "=", fixed = TRUE)

    unl <- unlist(n)
    if (length(unl) == 2) {
      return(unl[2])
    }
    return("")
  }, FUN.VALUE = character(1))

  names(values_opts) <- tolower(names_opts)
  urlsplit <- modifyList(urlsplit, as.list(values_opts))
  return(urlsplit)
}

eus_get_md5 <- function(x) {
  tmp <- tempfile(fileext = ".txt")
  writeLines(x, tmp)
  md5 <- unname(tools::md5sum(tmp))

  return(md5)
}

#' Get tiles from WMS
#' @importFrom utils download.file
#' @param transparent Transparency
#' @param bbox_expand Expansion of the bbox
#' @param url_pieces Provider
#' @inheritParams esp_getTiles
#'
#' @noRd

getwms <- function(newbbox,
                   url_pieces,
                   update_cache,
                   cache_dir,
                   verbose,
                   res,
                   transparent) {
  # Get squared bbox
  bbox <- as.double(sf::st_bbox(newbbox))
  dimx <- (bbox[3] - bbox[1])
  dimy <- (bbox[4] - bbox[2])
  maxdist <- max(dimx, dimy)
  center <- c(bbox[1] + dimx / 2, bbox[2] + dimy / 2)

  bboxsquare <- c(
    center[1] - maxdist / 2,
    center[2] - maxdist / 2,
    center[1] + maxdist / 2,
    center[2] + maxdist / 2
  )

  class(bboxsquare) <- "bbox"

  # Compose params
  url_pieces$bbox <- paste0(bboxsquare, collapse = ",")
  url_pieces$width <- as.character(res)
  url_pieces$height <- as.character(res)

  # Compose
  ext <- tolower(gsub("image/", "", url_pieces$format))
  if (!ext %in% c(
    "png", "jpeg", "jpg", "tiff",
    "geotiff", "png; mode=8bit"
  )) {
    stop(
      "Can't handle ", ext,
      " files"
    )
  }

  q <- url_pieces$q
  rest <- url_pieces[names(url_pieces) != "q"]
  q <- paste0(q, paste0(names(rest), "=", rest, collapse = "&"))

  crs <- unlist(
    url_pieces[names(url_pieces) %in% c("crs", "srs", "tilematrixset")]
  )

  filename <-
    paste0(
      "tile_",
      "_bbox_", crs,
      "_res",
      res,
      "_",
      paste0(bboxsquare, collapse = "_"),
      ".",
      ext
    )

  filename <- paste0(eus_get_md5(filename), ".", ext)
  filename <- file.path(cache_dir, filename)

  if (isFALSE(file.exists(filename)) || isTRUE(update_cache)) {
    if (verbose) {
      message("Downloading from \n", q, "\n to cache dir \n", cache_dir)
    }

    download.file(
      url = q,
      destfile = filename,
      mode = "wb",
      quiet = !verbose
    )
  } else {
    if (verbose) {
      message("Requested tile already cached on \n", cache_dir)
    }
  }

  # Read png and geotag
  # Only png
  if (ext == "png") {
    img <- png::readPNG(filename) * 255
  } else {
    img <- filename
  }

  # compose brick raster
  r_img <- suppressWarnings(terra::rast(img))

  # Provide transparency if available
  if (terra::nlyr(r_img) == 4 && transparent) {
    tomask <- terra::subset(r_img, 4)
    tomask[tomask == 0] <- NA

    r_img <- terra::mask(r_img, tomask)
  }

  # Check if need extent, some tiffs dont
  if (all(as.vector(terra::ext(r_img)) == c(0, res, 0, res))) {
    terra::ext(r_img) <- terra::ext(bboxsquare[c(1, 3, 2, 4)])
  }

  # Check if need a CRS
  if (terra::crs(r_img) == "") {
    terra::crs(r_img) <- crs
  }
  # End WMS

  return(r_img)
}

#' Get tiles from WMTS
#'
#' @inheritParams esp_getTiles
#' @inheritParams getWMS
#'
#' @noRd

getwmts <- function(newbbox,
                    type,
                    url_pieces,
                    update_cache,
                    cache_dir,
                    verbose,
                    zoom,
                    zoommin,
                    transparent,
                    extra_opts) {
  newbbox <- sf::st_transform(newbbox, 4326)
  bbx <- sf::st_bbox(newbbox)

  # select a default zoom level

  if (is.null(zoom)) {
    gz <- slippymath::bbox_tile_query(bbx)
    zoom <- min(gz[gz$total_tiles %in% 4:10, "zoom"]) + zoommin

    if (verbose) {
      message("Auto zoom level: ", zoom)
    }
  }

  # Check zoom
  if ("minzoom" %in% names(extra_opts)) {
    minzoom <- as.double(extra_opts$minzoom)

    if (zoom < minzoom) {
      zoom <- max(zoom, minzoom)
      if (verbose) {
        message(
          "\nSwitching. Minimum zoom for this provider is ",
          zoom,
          "\n"
        )
      }
    }
  }

  # get tile list
  tile_grid <- slippymath::bbox_to_tile_grid(
    bbox = bbx,
    zoom = as.numeric(zoom)
  )

  # Compose
  # Special case for non INSPIRE serves
  if (is.null(url_pieces$format)) {
    ext <- tools::file_ext(url_pieces$q)
  } else {
    ext <- tolower(gsub("image/", "", url_pieces$format))
  }
  if (!ext %in% c(
    "png", "jpeg", "jpg", "tiff",
    "geotiff"
  )) {
    stop(
      "Can't handle ", ext,
      " files"
    )
  }
  url_pieces$tilematrixset <- "GoogleMapsCompatible"
  url_pieces$tilematrix <- "{z}"
  url_pieces$tilerow <- "{y}"
  url_pieces$tilecol <- "{x}"

  q <- url_pieces$q
  rest <- url_pieces[names(url_pieces) != "q"]

  # Special case WMTS
  if (isFALSE(grepl("?", url_pieces$q, fixed = TRUE))) {
    q <- url_pieces$q
  } else {
    q <- paste0(q, paste0(names(rest), "=", rest, collapse = "&"))
  }

  if (verbose) {
    message("Caching tiles on ", cache_dir)
  }

  # download images
  images <- apply(
    X = tile_grid$tiles,
    MARGIN = 1,
    FUN = dl_t,
    z = tile_grid$zoom,
    ext = ext,
    src = type,
    q = q,
    verbose = verbose,
    cache_dir = cache_dir,
    update_cache = update_cache
  )

  rout <- compose_tile_grid(tile_grid, ext, images, transparent, "epsg:3857")
  return(rout)
}


#' @name compose_tile_grid
#' @noRd

compose_tile_grid <- function(tile_grid, ext, images, transparent, crs) {
  # Based on https://github.com/riatelab/maptiles/blob/main/R/get_tiles.R

  bricks <- vector("list", nrow(tile_grid$tiles))

  for (i in seq_along(bricks)) {
    bbox <- slippymath::tile_bbox(
      tile_grid$tiles$x[i], tile_grid$tiles$y[i],
      tile_grid$zoom
    )
    img <- images[i]

    # Read png and geotag
    # Only png
    if (ext == "png") {
      img <- png::readPNG(img) * 255
    }

    # compose brick raster
    r_img <- suppressWarnings(terra::rast(img))

    # Provide transparency if available
    if (terra::nlyr(r_img) == 4 && transparent) {
      tomask <- terra::subset(r_img, 4)
      tomask[tomask == 0] <- NA

      r_img <- terra::mask(r_img, tomask)
    }

    terra::ext(r_img) <- terra::ext(bbox[c(
      "xmin", "xmax",
      "ymin", "ymax"
    )])

    # Check if need a CRS
    if (terra::crs(r_img) == "") {
      terra::crs(r_img) <- crs
    }
    bricks[[i]] <- r_img
  }

  # if only one tile is needed
  if (length(bricks) == 1) {
    rout <- bricks[[1]]
    rout <- terra::merge(rout, rout)
  } else {
    # all tiles together
    rout <- do.call(terra::merge, bricks)
  }
  return(rout)
}


#' @name dl_t
#' @importFrom utils download.file
#' @noRd

dl_t <- function(x, z, ext, src, q, verbose, cache_dir, update_cache) {
  outfile <- paste0(
    cache_dir, "/", src, "_", z, "_", x[1], "_", x[2], ".",
    ext
  )

  if (!file.exists(outfile) || isTRUE(update_cache)) {
    q <- gsub(pattern = "{x}", replacement = x[1], x = q, fixed = TRUE)
    q <- gsub(pattern = "{y}", replacement = x[2], x = q, fixed = TRUE)
    q <- gsub(pattern = "{z}", replacement = z, x = q, fixed = TRUE)
    if (verbose) {
      message("Downloading ", q, "\n")
    }
    download.file(
      url = q,
      destfile = outfile,
      quiet = TRUE,
      mode = "wb"
    )
  } else if (verbose) {
    message("Tile cached on ", outfile)
  }
  return(outfile)
}

eus_hlp_get_bbox <- function(x, bbox_expand = 0.05, typeprov = "WMS") {
  bbox <- as.double(sf::st_bbox(x))
  dimx <- (bbox[3] - bbox[1])
  dimy <- (bbox[4] - bbox[2])
  center <- c(bbox[1] + dimx / 2, bbox[2] + dimy / 2)

  bbox_expand <- 1 + bbox_expand
  if (typeprov == "WMS") {
    maxdist <- max(dimx, dimy)
    dimy <- maxdist
    dimx <- dimy
  }
  newbbox <- c(
    center[1] - bbox_expand * dimx / 2,
    center[2] - bbox_expand * dimy / 2,
    center[1] + bbox_expand * dimx / 2,
    center[2] + bbox_expand * dimy / 2
  )

  class(newbbox) <- "bbox"
  newbbox <- sf::st_as_sfc(newbbox)
  sf::st_crs(newbbox) <- sf::st_crs(x)

  return(newbbox)
}
