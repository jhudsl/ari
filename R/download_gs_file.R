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
    cli::cli_alert_warning(
      paste("Is link sharing enabled?",
            "It's possible that this presentation isn't accessible.")
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

# Constructs an URL to export to pptx
pptx_url = function(id) {
  export_url(id, page_id = NULL, type = "pptx")
}

# Constructs an URL to export to pdf
pdf_url = function(id) {
  export_url(id, page_id = NULL, type = "pdf")
}

#' @export
#' @rdname get_slide_id
make_slide_url <- function(x) {
  x = get_slide_id(x)
  x = paste0("https://docs.google.com/presentation/d/",x)
  x
}

# Extract page IDs of slides in a Google Slides presentation
#' @importFrom jsonlite fromJSON
#' @import httr
get_page_ids = function(id) {
  id = get_slide_id(id)
  url = paste0("https://docs.google.com/presentation/d/", id)
  tfile = tempfile(fileext = ".html")
  res = httr::GET(url, httr::write_disk(tfile))
  httr::stop_for_status(res)
  cr = httr::content(res)
  script = rvest::html_nodes(cr, xpath ="//script")
  script = rvest::html_text(script)
  script = unique(script)
  script = gsub("DOCS_modelChunk = undefined;", "", script)
  script = script[ grepl("DOCS_modelChunk\\s=\\s\\[", x = script)]

  all_types = c("PREDEFINED_LAYOUT_UNSPECIFIED",
                "BLANK",
                "CAPTION_ONLY",
                "TITLE",
                "TITLE_AND_BODY",
                "TITLE_AND_TWO_COLUMNS",
                "TITLE_ONLY",
                "SECTION_HEADER",
                "SECTION_TITLE_AND_DESCRIPTION",
                "ONE_COLUMN_TEXT",
                "MAIN_POINT",
                "BIG_NUMBER",
                paste0("CUSTOM_", 1:100))
  types = paste0(all_types, collapse = "|")
  # script = script[grepl(types, script)]
  ss = strsplit(script, "; DOC")
  ss = lapply(ss, trimws)
  ss = lapply(ss, function(x) {
    x[!grepl("^DOC", x)] = paste0(" DOC", x[!grepl("^DOC", x)])
    x
  })
  ss = lapply(ss, function(x) {
    x = x[grepl("^DOCS_modelChunk\\s=\\s\\[", x)]
    x = x[ !x %in% "DOCS_modelChunk = undefined"]
    x = sub("^DOCS_modelChunk\\s=\\s\\[", "[", x)
    x
  })
  ss = unlist(ss)
  pages = lapply(ss, jsonlite::fromJSON)
  pages = sapply(pages, function(x) {
    x = x[sapply(x, function(r) any(unlist(r) %in% all_types))]
    x = x[length(x)]
    x
  })
  pages = sapply(pages, function(x) {
    if (length(x) < 2) {
      if (length(x) == 0) {
        return(NA)
      }
      x = x[[1]]
      if (length(x) < 2) {
        return(NA)
      }
    }
    x[[2]]
  })
  pages = pages[ !is.na(pages) ]
  if (length(pages) >= 2) {
    pages = c(pages[1], grep("^g", pages[2:length(pages)], value = TRUE))
  }
  if (pages[1] != "p") {
    pages = unique(c("p", pages))
  }
  urls = export_url(id = id, page_id = pages)
  pages = pages[check_png_urls(urls)]
  pages
}

#' @rdname get_slide_id
#' @export
#' @examples
#' x = "https://drive.google.com/drive/folders/1pXBQQdd1peI56GtQT-jEZ59xSmhqQlFC?usp=sharing"
#' get_folder_id(x)
#' x = "1pXBQQdd1peI56GtQT-jEZ59xSmhqQlFC"
#' get_folder_id(x)
get_folder_id = function(x) {
  res = httr::parse_url(x)
  x = res$path
  x = sub(".*folders/", "", x)
  x = sub("[?].*", "", x)
  x = x[ nchar(x) > 5]
  x = trimws(x)
  x
}

#' Convert a PDF file to a series of PNG image files
#'
#' Uses `pdftools::pdf_convert()` for conversion.
#'
#' @param path Path to the PDF file that needs to be converted to PNGs.
#' @param verbose A logical value indicating whether to display progress
#'   messages during the conversion process. The default value is TRUE
#' @param dpi The resolution in dots per inch (dpi) to be used for the PNG
#'   images. The default value is 600.
#'
#' @importFrom pdftools poppler_config pdf_info pdf_convert
pdf_to_pngs = function(path,
                       verbose = TRUE,
                       dpi = 600) {
  fmts = pdftools::poppler_config()$supported_image_formats
  if ("png" %in% fmts) {
    format = "png"
  } else {
    format = fmts[1]
  }
  info = pdftools::pdf_info(pdf = path)
  filenames = vapply(seq.int(info$pages), function(x) {
    tempfile(fileext = paste0(".", format))
  }, FUN.VALUE = character(1))
  if (verbose) {
    message("Converting PDFs to PNGs")
  }
  pngs = pdftools::pdf_convert(
    pdf = path, dpi = dpi,
    format = format, filenames = filenames,
    verbose = as.logical(verbose))
  pngs
}
