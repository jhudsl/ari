#' Generate video from images and audio
#'
#' Given a vector of paths to images (preferably \code{.jpg}s
#' or \code{.png}s) and a flat list of \code{\link[tuneR]{Wave}}s of equal
#' length this function will create an \code{.mp4} video file where each image
#' is shown with its corresponding audio. Take a look at the
#' \code{\link[tuneR]{readWave}} function if you want to import your audio
#' files into R. Please be sure that all images have the same dimensions.
#'
#' This function uses \href{https://ffmpeg.org/}{FFmpeg}
#' which you should be sure is installed before using this function. If running
#' \code{Sys.which("ffmpeg")} in your R console returns an empty string after
#' installing FFmpeg then you should set the path to FFmpeg on you computer to
#' an environmental variable using \code{Sys.setenv(ffmpeg = "path/to/ffmpeg")}.
#' The environmental variable will always override the result of
#' \code{Sys.which("ffmpeg")}.
#'
#' @param images A vector of paths to images.
#' @param audio A list of \code{Wave}s from tuneR.
#' @param output A path to the video file which will be created.
#' @param verbose print diagnostic messages.  If > 1, then more are printed
#' @param cleanup If \code{TRUE}, interim files are deleted
#' @param ffmpeg_args Ffmpeg arguments set by \code{set_ffmpeg_args()}
#' @param duration a vector of numeric durations for each audio
#' track. See \code{\link{pad_wav}}
#' @param check_inputs Should the inputs be checked?  Almost always should
#' be \code{TRUE}, but may be useful if trying to do customized stuff.
#' @return A logical value, with the attribute \code{outfile} for the
#' output file.

#' @importFrom purrr reduce discard
#' @importFrom tuneR bind writeWave
#' @export
#' @examples
#' \dontrun{
#' if (ffmpeg_version_sufficient()) {
#'   result <- ari_stitch(
#'     ari_example(c("mab1.png", "mab2.png")),
#'     list(tuneR::noise(), tuneR::noise())
#'   )
#'   result <- ari_stitch(
#'     ari_example(c("mab1.png", "mab2.png")),
#'     list(tuneR::noise(), tuneR::noise()),
#'     ffmpeg_opts = "-qscale 0",
#'     verbose = 2
#'   )
#'   # system2("open", attributes(result)$outfile)
#' }
#' }
ari_stitch <- function(images, audio,
                       output,
                       verbose = FALSE,
                       cleanup = TRUE,
                       ffmpeg_args = set_ffmpeg_args(),
                       duration = NULL,
                       check_inputs = TRUE) {
  # File path processing
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  output <- file.path(output_dir, basename(output))
  stopifnot(length(audio) > 0, dir.exists(output_dir))
  # Input check
  if (check_inputs) {
    stopifnot(identical(length(images), length(audio)),
              all(file.exists(images)))
  }
  # If audio is filename instead of Wave object
  if (is.character(audio)) {
    audio <- lapply(audio, function(x) {
      ext <- tolower(tools::file_ext(x))
      func <- switch(ext,
                     wav = tuneR::readWave,
                     mp3 = tuneR::readMP3,
                     tuneR::readMP3)
      func(x)
    })
    audio <- pad_wav(audio, duration = duration)
  }

  if (verbose > 0) {
    message("Writing out Wav for audio")
  }
  if (verbose > 1) {
    print(audio)
  }

  # Audio preprocessing
  audio <- match_sample_rate(audio, verbose = verbose)
  wav <- purrr::reduce(audio, tuneR::bind)
  wav_path <- file.path(output_dir, paste0("ari_audio_", get_random_string(), ".wav"))
  tuneR::writeWave(wav, filename = wav_path)
  if (cleanup) {
    on.exit(unlink(wav_path, force = TRUE), add = TRUE)
  }

  # If any gif images, convert all images to gif
  img_ext <- tolower(tools::file_ext(images))
  any_gif <- any(img_ext %in% "gif")
  if (any_gif & !all(img_ext %in% "gif")) {
    if (verbose > 0) {
      message("Converting All files to gif!")
    }
    for (i in seq_along(images)) {
      iext <- img_ext[i]
      if (iext != "gif") {
        tfile <- tempfile(fileext = ".gif")
        ffmpeg_convert(images[i], outfile = tfile)
        images[i] <- tfile
      }
    }
  }

  ## on windows ffmpeg cancats names adding the working directory, so if
  ## complete url is provided it adds it twice.
  if (.Platform$OS.type == "windows") {
    new_image_names <- file.path(output_dir, basename(images))
    if (!any(file.exists(new_image_names))) {
      file.copy(images, to = new_image_names)
    } else {
      warning("On windows must make basename(images) for ffmpeg to work")
    }
    images <- basename(images)
  }

  # Add "file 'IMAGE_PATH'" and duration in txt file located at input_txt_path
  input_txt_path <- file.path(
    output_dir,
    paste0(
      "ari_input_",
      get_random_string(),
      ".txt"
    )
  )
  for (i in seq_along(images)) {
    cat(paste0("file ", "'", images[i], "'", "\n"),
        file = input_txt_path,
        append = TRUE)
    cat(paste0("duration ", wav_length(audio[[i]]), "\n"),
        file = input_txt_path,
        append = TRUE)
  }
  # duplicate last entry
  cat(paste0("file ", "'", images[length(images)], "'", "\n"),
      file = input_txt_path,
      append = TRUE)

  # define separator to be used on Windows
  input_txt_path <- normalizePath(input_txt_path, winslash = "/")

  # Build ffmpeg command to input into system()
  command <- build_ffmpeg_command(input_txt_path, wav_path, ffmpeg_args, output)

  if (verbose > 0) {
    message(command)
  }
  if (verbose > 1) {
    message("Input text path is:")
    cat(readLines(input_txt_path), sep = "\n")
  }

  # run command in system
  res <- system(command)
  # check if result was non-zero for ffmpeg
  if (res != 0) {
    warning("Result was non-zero for ffmpeg")
  }

  if (cleanup) {
    on.exit(unlink(input_txt_path, force = TRUE), add = TRUE)
  }
  res <- file.exists(output) && file.size(output) > 0
  if (!cleanup) {
    attr(res, "txt_path") <- input_txt_path
    attr(res, "wav_path") <- wav_path
    attr(res, "cmd") <- command
  }
  attr(res, "outfile") <- output
  attr(res, "images") <- images
  invisible(res)
}
