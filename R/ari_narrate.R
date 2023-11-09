#' Generate video from slides and a script
#'
#' \code{ari_narrate} creates a video from a script written in markdown and HTML
#' slides created with \code{\link[rmarkdown]{rmarkdown}} or a similar package.
#'
#' @param script Either a markdown file where every paragraph will be read over
#' a corresponding slide, or an \code{.Rmd} file where each HTML comment will
#' be used for narration.
#' @param slides A path or URL for an HTML slideshow created with
#' \code{\link[rmarkdown]{rmarkdown}}, \code{xaringan}, or a
#' similar package.
#' @param output The path to the video file which will be created.
#' @param tts_engine The desired engine for converting text-to-speech
#' @param tts_engine_args List of parameters provided to the designated text-to-speech engine
#' @param tts_engine_auth Authentication required for the designated text-to-speech engine
#' @param capture_method Either \code{"vectorized"} or \code{"iterative"}.
#' The vectorized mode is faster though it can cause screens to repeat. If
#' making a video from an \code{\link[rmarkdown]{ioslides_presentation}}
#' you should use \code{"iterative"}.
#' @param subtitles Should a \code{.srt} file be created with subtitles? The
#' default value is \code{FALSE}. If \code{TRUE} then a file with the same name
#' as the \code{output} argument will be created, but with the file extension
#' \code{.srt}.
#' @param verbose print diagnostic messages.  If > 1, then more are printed
#' @param cleanup If \code{TRUE}, interim files are deleted
#' @param ... Arguments that will be passed to \code{\link[webshot]{webshot}}.
#'
#' @return The output from \code{\link{ari_spin}}
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom rmarkdown render html_document
#' @importFrom purrr map_chr walk
#' @importFrom webshot webshot
#' @importFrom tools file_ext
#' @export
#' @examples
#' \dontrun{
#' # Narrate example slides with script
#' ari_narrate(system.file("test", "ari_intro_script.md", package = "ari"),
#'             system.file("test", "ari_intro.html", package = "ari"),
#'             output = "test.mp4",
#'              tts_engine = text2speech::tts,
#'              tts_engine_args = coqui_args(),
#'              tts_engine_auth = text2speech::tts_auth)
#' }
ari_narrate <- function(script, slides, output,
                        tts_engine = text2speech::tts,
                        tts_engine_args = coqui_args(),
                        tts_engine_auth = text2speech::tts_auth,
                        capture_method = c("vectorized", "iterative"),
                        subtitles = FALSE,
                        verbose = FALSE,
                        cleanup = TRUE,
                        ...) {
  # Authentication for Text-to-Speech Engines
  auth <- tts_engine_auth(service =  tts_engine_args$service)
  # Stop message
  if (!auth) {
    stop(paste0(
      "It appears you're not authenticated with ",
      tts_engine_args$service, ". Make sure you've ",
      "set the appropriate environmental variables ",
      "before you proceed."
    ))
  }
  # Check capture_method
  capture_method <- match.arg(capture_method)
  if (!(capture_method %in% c("vectorized", "iterative"))) {
    stop('capture_method must be either "vectorized" or "iterative"')
  }
  # Output directory, path to script
  output_dir <- normalizePath(dirname(output))
  script <- normalizePath(script)
  if (file_ext(script) %in% c("Rmd", "rmd") & missing(slides)) {
    tfile <- tempfile(fileext = ".html")
    slides <- rmarkdown::render(input = script, output_file = tfile)
  }
  # Slides
  if (file.exists(slides)) {
    slides <- normalizePath(slides)
    if (.Platform$OS.type == "windows") {
      slides <- paste0("file://localhost/", slides)
    } else {
      slides <- paste0("file://localhost", slides)
    }
  }
  # Check if script and output_dir exists
  stopifnot(
    file.exists(script),
    dir.exists(output_dir)
  )
  # Convert script to html and get text
  if (file_ext(script) %in% c("Rmd", "rmd")) {
    paragraphs <- parse_html_comments(script)
  } else {
    html_path <- file.path(output_dir, paste0("ari_script_", get_random_string(), ".html"))
    if (cleanup) {
      on.exit(unlink(html_path, force = TRUE), add = TRUE)
    }
    rmarkdown::render(script, output_format = rmarkdown::html_document(), output_file = html_path)
    paragraphs <- map_chr(
      rvest::html_text(rvest::html_nodes(xml2::read_html(html_path), "p")),
      function(x) {
        gsub("\u2019", "'", x)
      }
    )
  }
  # Path to images
  slide_nums <- seq_along(paragraphs)
  img_paths <- file.path(
    output_dir,
    paste0(
      "ari_img_",
      slide_nums, "_",
      get_random_string(), ".jpeg"
    )
  )
  # Take screenshot
  if (capture_method == "vectorized") {
    webshot::webshot(url = paste0(slides, "#", slide_nums), file = img_paths, ...)
  } else {
    for (i in slide_nums) {
      webshot::webshot(url = paste0(slides, "#", i), file = img_paths[i], ...)
    }
  }

  if (cleanup) {
    on.exit(walk(img_paths, unlink, force = TRUE), add = TRUE)
  }

  # Pass along ari_spin()
  ari_spin(
    images = img_paths, paragraphs = paragraphs,
    output = output,
    tts_engine = tts_engine,
    tts_engine_args =  tts_engine_args,
    tts_engine_auth = tts_engine_auth,
    subtitles = subtitles)
}
