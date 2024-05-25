catreus_read_atom <- function(file, encoding = "UTF-8") {
  feed <- try(
    xml2::as_list(xml2::read_xml(file,
                                 options = "NOCDATA",
                                 encoding = encoding
    )),
    silent = TRUE
  )

  # On error try without encoding
  if (inherits(feed, "try-error")) {
    feed <- xml2::as_list(xml2::read_xml(file,
                                         options = "NOCDATA"
    ))
  }

  # Prepare data
  feed <- feed$feed
  feed <- feed[names(feed) == "entry"]

  tbl_all <- lapply(feed, function(x) {
    title <- unlist(x$title)
    url <- unlist(attr(x$link, "href"))
    date <- as.POSIXct(unlist(feed[1]$entry$updated))
    data.frame(
      title = trimws(title),
      url = trimws(url),
      date = as.Date(date)
    )
  })


  tbl_all <- dplyr::bind_rows(tbl_all)
  tbl_all$title <- gsub(
    "Download INSPIRE of the municipality ", "",
    tbl_all$title
  )
  tbl_all <- dplyr::as_tibble(tbl_all)
  return(tbl_all)
}
