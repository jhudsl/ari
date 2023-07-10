#' Create a video from images and text
#'
#' Given equal length vectors of paths to images (preferably \code{.jpg}s
#' or \code{.png}s) and strings which will be
#' synthesized by
#' \href{https://aws.amazon.com/polly/}{Amazon Polly} or
#' any other synthesizer available in
#' \code{\link[text2speech]{tts}}, this function creates an
#' \code{.mp4} video file where each image is shown with
#' its corresponding narration. This function uses \code{\link{ari_stitch}} to
#' create the video.
#'
#' This function needs to connect to
#' \href{https://aws.amazon.com/}{Amazon Web Services} in order to create the
#' narration. You can find a guide for accessing AWS from R
#' \href{http://seankross.com/2017/05/02/Access-Amazon-Web-Services-in-R.html}{here}.
#' For more information about how R connects
#' to Amazon Polly see the \code{aws.polly} documentation
#' \href{https://github.com/cloudyr/aws.polly}{here}.
#'
#' @param images A vector of paths to images.
#' @param paragraphs A vector strings that will be spoken by Amazon Polly.
#' @param output A path to the video file which will be created.
#' @param voice The voice you want to use. See
#' \code{\link[text2speech]{tts_voices}} for more information
#' about what voices are available.
#' @param model_name (Coqui only) Text-to-speech model
#' @param vocoder_name (Coqui only) Vocoder (Voice Coder) model
#' @param service speech synthesis service to use,
#' passed to \code{\link[text2speech]{tts}},
#' Either \code{"amazon"}, \code{"microsoft"}, or \code{"google"}.
#' @param subtitles Should a \code{.srt} file be created with subtitles? The
#' default value is \code{FALSE}. If \code{TRUE} then a file with the same name
#' as the \code{output} argument will be created, but with the file extension
#' \code{.srt}.
#' @param duration a vector of numeric durations for each audio
#' track.  See \code{\link{pad_wav}}
#' @param ... additional arguments to \code{\link{ari_stitch}}
#' @param tts_args list of arguments to pass to \code{\link{tts}}
#' @param key_or_json_file access key or JSON file to pass to
#' \code{\link{tts_auth}} for authorization
#'
#' @return The output from \code{\link{ari_stitch}}
#'
#' @importFrom text2speech tts_auth tts tts_default_voice
#' @importFrom tuneR bind Wave
#' @importFrom purrr map reduce
#' @importFrom progress progress_bar
#' @importFrom tools file_path_sans_ext
#' @importFrom cli cli_alert_info
#' @export
#' @examples
#' \dontrun{
#'
#' slides <- system.file("test", c("mab2.png", "mab1.png"),
#'   package = "ari"
#' )
#' sentences <- c(
#'   "Welcome to my very interesting lecture.",
#'   "Here are some fantastic equations I came up with."
#' )
#' ari_spin(slides, sentences, voice = "Joey")
#' }
#'
ari_spin <- function(images, paragraphs,
                     output = tempfile(fileext = ".mp4"),
                     voice = text2speech::tts_default_voice(service = service),
                     model_name = "tacotron2-DDC_ph",
                     vocoder_name = "ljspeech/univnet",
                     service = ifelse(have_polly(), "amazon", "google"),
                     subtitles = FALSE,
                     duration = NULL,
                     tts_args = NULL,
                     key_or_json_file = NULL,
                     ...) {
  # Check for ffmpeg
  ffmpeg_exec()

  # Argument checks
  auth <- text2speech::tts_auth(
    service = service,
    key_or_json_file = key_or_json_file
  )
  if (!auth) {
    stop(paste0(
      "It appears you're not authenticated with ",
      service, ". Make sure you've ",
      "set the appropriate environmental variables ",
      "before you proceed."
    ))
  }
  # Create file path to output
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))

  if (length(paragraphs) == 1) {
    if (file.exists(paragraphs)) {
      paragraphs <- readLines(paragraphs, warn = FALSE)
      paragraphs <- paragraphs[!paragraphs %in% ""]
    }
  }

  semi_colon <- trimws(paragraphs) == ";"
  if (any(semi_colon)) {
    warning(paste0(
      "Some paragraphs are simply a semicolon - ",
      "likely needs to be replaced or slide removed!"
    ))
  }
  stopifnot(
    length(paragraphs) > 0,
    identical(length(images), length(paragraphs)),
    all(file.exists(images)),
    dir.exists(output_dir)
  )
  # End of Argument checks

  # Setup objects to populate in for-loop with tts()
  wave_objects <- vector(mode = "list", length = length(paragraphs))
  par_along <- seq_along(paragraphs)
  ideal_duration <- rep(NA, length(paragraphs))

  pb <- progress_bar$new(
    format = "Fetching Narration [:bar] :percent",
    total = length(par_along)
  )
  if (service == "coqui") {
    cli::cli_alert_info("Coqui TTS does not support MP3 format; will produce a WAV audio output.")
  }

  # Iterate through arguments used in tts()
  for (i in par_along) {
    args <- tts_args
    args$text <- paragraphs[i]
    args$voice <- voice
    args$service <- service
    args$bind_audio <- TRUE
    # coqui+ari doesn't work with mp3
    if (service == "coqui") {
      args$output_format <- "wav"
      args$voice <- NULL
      args$model_name <- model_name
      args$vocoder_name <- vocoder_name
    }
    wav <- do.call(text2speech::tts, args = args)
    wav <- reduce(wav$wav, bind)
    wav <- pad_wav(wav, duration = duration[i])
    ideal_duration[i] <- length(wav@left) / wav@samp.rate
    wave_objects[[i]] <- wav
    pb$tick()
  }


  # Burn subtitles
  if (subtitles) {
    sub_file <- paste0(file_path_sans_ext(output), ".srt")
    ari_subtitles(paragraphs, wave_objects, sub_file)
  }

  # Create a video from images and audio
  res <- ari_stitch(images, wave_objects, output, ...)

  # Collect output
  args <- list(...)
  cleanup <- args$cleanup
  if (is.null(cleanup)) {
    cleanup <- TRUE
  }
  if (!cleanup) {
    attr(res, "wavs") <- wave_objects
  }
  attr(res, "voice") <- voice
  if (subtitles) {
    attr(res, "subtitles") <- sub_file
  }
  attr(res, "service") <- service
  return(res)
}

#' @rdname ari_spin
#' @export
have_polly <- function() {
  requireNamespace("aws.polly", quietly = TRUE)
}
