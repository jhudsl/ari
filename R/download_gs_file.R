#' Download Google Slides File
#'
#' @param gs_url Link to Google slides presentation, passed to
#' \code{\link{get_slide_id}}
#' @param out_type output type of file to download. Usually
#' `pdf` or `pptx`
#'
#' @note This downloads presentations if they are public and also try to make
#' sure it does not fail on large files
#' @return Downloaded file (in temporary directory)
#' @export
download_gs_file = function(gs_url, out_type = "pptx") {
  stopifnot(is.character(gs_url))
  id = get_slide_id(gs_url)
  # construct URL to export image file from Google Slides
  url = export_url(id = id, page_id = NULL, type = out_type)
  tmp = tempfile(fileext = paste0(".", out_type))

  # retrieve from url and write response to disk
  result = httr::GET(url, httr::write_disk(tmp))
  warn_user = FALSE
  fr_header = result$headers$`x-frame-options`
  if (!is.null(fr_header)) {
    if (all(fr_header == "DENY")) {
      warn_user = TRUE
    }
  }
  if (httr::status_code(result) >= 300) {
    warn_user = TRUE
  }
  # Don't write something if not really a pptx
  content_type = result$headers$`content-type`
  if (httr::status_code(result) >= 400 &&
      !is.null(content_type) && grepl("html", content_type)) {
    file.remove(tmp)
  }
  if (grepl("ServiceLogin", result$url)) {
    warn_user = TRUE
  }

  if (warn_user) {
    warning(
      paste0(
        "This presentation may not be available, ",
        "did you turn link sharing on?")
    )
  }
  tmp
}

#' Get Slide ID from URL
#'
#' @param x URL of slide
#'
#' @return A character vector
#' @export
#'
#' @examples
#' x = paste0("https://docs.google.com/presentation/d/",
#' "1Tg-GTGnUPduOtZKYuMoelqUNZnUp3vvg_7TtpUPL7e8",
#' "/edit#slide=id.g154aa4fae2_0_58")
#' get_slide_id(x)
get_slide_id <- function(x) {
  x = sub(".*presentation/", "", x)
  x = sub("/d/e", "/d", x) # if you publish by accident
  x = sub("^(d|e)/", "", x)
  x = strsplit(x, "/")[[1]]
  x = x[ !grepl("^(edit|pub|export|png)", x)]
  x = x[ nchar(x) > 5]
  x
}

# Constructs an URL to export an image file from a Google Slides
export_url <- function(id, page_id = NULL, type = "png") {
  url <- paste0("https://docs.google.com/presentation/d/",
                id, "/export/", type, "?id=", id)
  if (!is.null(page_id)) {
    url = paste0(url, "&pageid=", page_id)
  }
  url
}
