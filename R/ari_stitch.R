#' Create a video from images and audio
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
#' @param duration a vector of numeric durations for each audio
#' track.  See \code{\link{pad_wav}}
#' @param output A path to the video file which will be created.
#' @param verbose print diagnostic messages.  If > 1, then more are printed
#' @param cleanup If \code{TRUE}, interim files are deleted
#' @param ffmpeg_opts additional options to send to \code{ffmpeg}.
#' This is an advanced option, use at your own risk
#' @param divisible_height Make height divisible by 2, which may
#' be required if getting "height not divisible by 2" error.
#' @param audio_codec The audio encoder for the splicing.  If this
#' fails, try \code{copy}.
#' @param video_codec The video encoder for the splicing.  If this
#' fails, see \code{ffmpeg -codecs}
#' @param audio_bitrate Bit rate for audio. Passed to \code{-b:a}.
#' @param video_bitrate Bit rate for video. Passed to \code{-b:v}.
#' @param video_sync_method Video sync method.  Should be
#' "auto" or `"vfr"` or a numeric.  See \url{https://ffmpeg.org/ffmpeg.html}.
#' @param pixel_format pixel format to encode for `ffmpeg`.
#' @param fast_start Adding `faststart` flags for YouTube and other sites,
#' see \url{https://trac.ffmpeg.org/wiki/Encode/YouTube}
#' @param deinterlace should the video be de-interlaced,
#' see \url{https://ffmpeg.org/ffmpeg-filters.html}, generally for
#' YouTube
#' @param frames_per_second frames per second of the video, should
#' be an integer
#' @param stereo_audio should the audio be forced to stereo,
#' corresponds to `-ac 2`
#' @param video_filters any options that are passed to \code{-vf} arguments
#' for \code{ffmpeg}
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
                       output = tempfile(fileext = ".mp4"),
                       verbose = FALSE,
                       cleanup = TRUE,
                       ffmpeg_opts = "",
                       divisible_height = TRUE,
                       audio_codec = get_audio_codec(),
                       video_codec = get_video_codec(),
                       video_sync_method = "2",
                       audio_bitrate = NULL,
                       video_bitrate = NULL,
                       pixel_format = "yuv420p",
                       fast_start = FALSE,
                       deinterlace = FALSE,
                       stereo_audio = TRUE,
                       duration = NULL,
                       video_filters = NULL,
                       frames_per_second = NULL,
                       check_inputs = TRUE) {
  # Argument check and file path processing
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  output <- file.path(output_dir, basename(output))
  stopifnot(
    length(audio) > 0,
    dir.exists(output_dir)
  )
  if (check_inputs) {
    stopifnot(
      identical(length(images), length(audio)),
      all(file.exists(images))
    )
  }
  if (is.character(audio)) {
    audio <- lapply(audio, function(x) {
      ext <- tolower(tools::file_ext(x))
      func <- switch(ext,
                     wav = tuneR::readWave,
                     mp3 = tuneR::readMP3,
                     tuneR::readMP3
      )
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
  # End of Argument check and file path processing

  # Audio preprocessing
  audio <- match_sample_rate(audio, verbose = verbose)
  wav <- purrr::reduce(audio, tuneR::bind)
  wav_path <- file.path(output_dir, paste0("ari_audio_", get_random_string(), ".wav"))
  tuneR::writeWave(wav, filename = wav_path)

  if (cleanup) {
    on.exit(unlink(wav_path, force = TRUE), add = TRUE)
  }

  # if there are any gif images, convert all images to gif
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

  input_txt_path <- file.path(
    output_dir,
    paste0(
      "ari_input_",
      get_random_string(),
      ".txt"
    )
  )
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

  # add "file 'IMAGE_PATH'" and duration in txt file located at input_txt_path
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

  # needed for users as per
  # https://superuser.com/questions/718027/
  # ffmpeg-concat-doesnt-work-with-absolute-path
  # input_txt_path = normalizePath(input_txt_path, winslash = "\\")

  ffmpeg <- ffmpeg_exec(quote = TRUE)

  # set video filters
  if (!is.null(frames_per_second)) {
    video_filters <- c(video_filters, paste0("fps=", frames_per_second))
  } else {
    video_filters <- c(video_filters, "fps=5")
  }
  if (divisible_height) {
    video_filters <- c(video_filters, '"scale=trunc(iw/2)*2:trunc(ih/2)*2"')
  }


  # workaround for older ffmpeg
  # https://stackoverflow.com/questions/32931685/
  # the-encoder-aac-is-experimental-but-experimental-codecs-are-not-enabled
  experimental <- FALSE
  if (!is.null(audio_codec)) {
    if (audio_codec == "aac") {
      experimental <- TRUE
    }
  }
  if (deinterlace) {
    video_filters <- c(video_filters, "yadif")
  }
  video_filters <- paste(video_filters, collapse = ",")
  video_filters <- paste0("-vf ", video_filters)

  if (any(grepl("-vf", ffmpeg_opts))) {
    warning("Found video filters in ffmpeg_opts, may not be used correctly!")
  }
  ffmpeg_opts <- c(video_filters, ffmpeg_opts)
  ffmpeg_opts <- paste(ffmpeg_opts, collapse = " ")

  # create ffmpeg command
  command <- paste(
    ffmpeg, "-y",
    "-f concat -safe 0 -i", shQuote(input_txt_path),
    "-i", shQuote(wav_path),
    ifelse(!is.null(video_codec), paste("-c:v", video_codec),
           ""
    ),
    ifelse(!is.null(audio_codec), paste("-c:a", audio_codec),
           ""
    ),
    ifelse(stereo_audio, "-ac 2", ""),
    ifelse(!is.null(audio_bitrate), paste("-b:a", audio_bitrate),
           ""
    ),
    ifelse(!is.null(video_bitrate), paste("-b:v", video_bitrate),
           ""
    ),
    # ifelse(deinterlace, "-vf yadif", ""),
    ifelse(!is.null(video_sync_method), paste("-fps_mode", "auto"),
           ""
    ),
    ifelse(!is.null(pixel_format), paste("-pix_fmt", pixel_format),
           ""
    ),
    ifelse(fast_start, "-movflags +faststart", ""),
    ffmpeg_opts,
    ifelse(!is.null(frames_per_second), paste0("-r ", frames_per_second), ""),
    ifelse(experimental, "-strict experimental", ""),
    "-max_muxing_queue_size 9999",
    "-threads 2",
    shQuote(output)
  )
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
