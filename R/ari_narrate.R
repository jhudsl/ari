#' Create a video from slides and a script
#'
#' \code{ari_narrate} creates a video from a script written in markdown and HTML
#' slides created with \code{\link[rmarkdown]{rmarkdown}} or a similar package.
#' This function uses \href{https://aws.amazon.com/polly/}{Amazon Polly}
#' via \code{\link{ari_spin}}.
#'
#' @param script Either a markdown file where every paragraph will be read over
#' a corresponding slide, or an \code{.Rmd} file where each HTML comment will
#' be used for narration.
#' @param slides A path or URL for an HTML slideshow created with
#' \code{\link[rmarkdown]{rmarkdown}}, \code{xaringan}, or a
#' similar package.
#' @param output The path to the video file which will be created.
#' @param voice The voice you want to use. See
#' \code{\link[text2speech]{tts_voices}} for more information
#' about what voices are available.
#' @param service speech synthesis service to use,
#' passed to \code{\link[text2speech]{tts}}.
#' Either \code{"amazon"} or \code{"google"}.
#' @param capture_method Either \code{"vectorized"} or \code{"iterative"}.
#' The vectorized mode is faster though it can cause screens to repeat. If
#' making a video from an \code{\link[rmarkdown]{ioslides_presentation}}
#' you should use \code{"iterative"}.
#' @param subtitles Should a \code{.srt} file be created with subtitles? The
#' default value is \code{FALSE}. If \code{TRUE} then a file with the same name
#' as the \code{output} argument will be created, but with the file extension
#' \code{.srt}.
#' @param ... Arguments that will be passed to \code{\link[webshot]{webshot}}.
#' @param verbose print diagnostic messages.  If > 1, then more are printed
#' @param audio_codec The audio encoder for the splicing.  If this
#' fails, try \code{copy}.
#' @param video_codec The video encoder for the splicing.  If this
#' fails, see \code{ffmpeg -codecs}
#' @param cleanup If \code{TRUE}, interim files are deleted
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
#'
#' #
#' ari_narrate(system.file("test", "ari_intro_script.md", package = "ari"),
#'   system.file("test", "ari_intro.html", package = "ari"),
#'   voice = "Joey"
#' )
#' }
ari_narrate <- function(script, slides,
                        output = tempfile(fileext = ".mp4"),
                        voice = text2speech::tts_default_voice(service = service),
                        service = "amazon",
                        capture_method = c("vectorized", "iterative"),
                        subtitles = FALSE, ...,
                        verbose = FALSE,
                        audio_codec = get_audio_codec(),
                        video_codec = get_video_codec(),
                        cleanup = TRUE) {
  auth <- text2speech::tts_auth(service = service)
  if (!auth) {
    stop(paste0(
      "It appears you're not authenticated with ",
      service, ". Make sure you've ",
      "set the appropriate environmental variables ",
      "before you proceed."
    ))
  }


  capture_method <- match.arg(capture_method)
  if (!(capture_method %in% c("vectorized", "iterative"))) {
    stop('capture_method must be either "vectorized" or "iterative"')
  }

  output_dir <- normalizePath(dirname(output))
  script <- normalizePath(script)
  if (file_ext(script) %in% c("Rmd", "rmd") & missing(slides)) {
    tfile <- tempfile(fileext = ".html")
    slides <- rmarkdown::render(input = script, output_file = tfile)
  }

  if (file.exists(slides)) {
    slides <- normalizePath(slides)
    if (.Platform$OS.type == "windows") {
      slides <- paste0("file://localhost/", slides)
    } else {
      slides <- paste0("file://localhost", slides)
    }
  }
  stopifnot(
    file.exists(script),
    dir.exists(output_dir)
  )

  if (file_ext(script) %in% c("Rmd", "rmd")) {
    paragraphs <- parse_html_comments(script)
  } else {
    html_path <- file.path(output_dir, paste0("ari_script_", grs(), ".html"))
    if (cleanup) {
      on.exit(unlink(html_path, force = TRUE), add = TRUE)
    }
    render(script, output_format = html_document(), output_file = html_path)
    paragraphs <- map_chr(
      html_text(html_nodes(read_html(html_path), "p")),
      function(x) {
        gsub("\u2019", "'", x)
      }
    )
  }

  slide_nums <- seq_along(paragraphs)
  img_paths <- file.path(
    output_dir,
    paste0(
      "ari_img_",
      slide_nums, "_",
      grs(), ".jpeg"
    )
  )

  if (capture_method == "vectorized") {
    webshot(url = paste0(slides, "#", slide_nums), file = img_paths, ...)
  } else {
    for (i in slide_nums) {
      webshot(url = paste0(slides, "#", i), file = img_paths[i], ...)
    }
  }

  if (cleanup) {
    on.exit(walk(img_paths, unlink, force = TRUE), add = TRUE)
  }
  ari_spin(
    images = img_paths, paragraphs = paragraphs,
    output = output, voice = voice,
    service = service, subtitles = subtitles,
    verbose = verbose, cleanup = cleanup
  )
}
