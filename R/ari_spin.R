#' Generate video from images and text
#'
#' Given equal length vectors of paths to images (preferably \code{.jpg}s
#' or \code{.png}s) and strings which will be
#' synthesized by a text-to-speech engine, this function creates an
#' \code{.mp4} video file where each image is shown with
#' its corresponding narration. This function uses \code{\link{ari_stitch}} to
#' create the video.
#'
#' @param images A vector of paths to images.
#' @param paragraphs A vector strings that will be spoken by Amazon Polly.
#' @param output A path to the video file which will be created.
#' @param tts_engine The desired engine for converting text-to-speech
#' @param tts_engine_args List of parameters provided to the designated text-to-speech engine
#' @param tts_engine_auth Authentication required for the designated text-to-speech engine
#' @param subtitles Should a \code{.srt} file be created with subtitles? The
#' default value is \code{FALSE}. If \code{TRUE} then a file with the same name
#' as the \code{output} argument will be created, but with the file extension
#' \code{.srt}.
#' @param duration a vector of numeric durations for each audio
#' track.  See \code{\link{pad_wav}}
#' @param key_or_json_file access key or JSON file to pass to
#' \code{\link{tts_auth}} for authorization
#'
#' @return The output from \code{\link{ari_stitch}}
#'
#' @importFrom text2speech tts_auth tts tts_default_voice
#' @importFrom tuneR bind Wave
#' @importFrom purrr map reduce
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
#' ari_spin(slides, sentences, output = "test.mp4",
#'          tts_engine_args = coqui_args(model_name = "tacotron2-DDC_ph",
#'           vocoder_name = "ljspeech/univnet"))
#' }
#'
ari_spin <- function(images, paragraphs, output,
                     tts_engine = text2speech::tts,
                     tts_engine_args = coqui_args(),
                     tts_engine_auth = text2speech::tts_auth,
                     subtitles = FALSE, duration = NULL, key_or_json_file = NULL) {
  # Check for ffmpeg
  ffmpeg_exec()
  # Argument checks
  auth <- tts_engine_auth(
    service = tts_engine_args$service,
    key_or_json_file = key_or_json_file
  )
  if (!auth) {
    stop(paste0(
      "It appears you're not authenticated with ",
      tts_engine_args$service, ". Make sure you've ",
      "set the appropriate environmental variables ",
      "before you proceed."
    ))
  }
  # Create file path to output
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  # Paragraphs
  if (length(paragraphs) == 1) {
    if (file.exists(paragraphs)) {
      paragraphs <- readLines(paragraphs, warn = FALSE)
      paragraphs <- paragraphs[!paragraphs %in% ""]
    }
  }
  # Paragraphs: Check for semicolons
  semi_colon <- trimws(paragraphs) == ";"
  if (any(semi_colon)) {
    warning(paste0(
      "Some paragraphs are simply a semicolon - ",
      "likely needs to be replaced or slide removed!"
    ))
  }
  # Check for arguments
  stopifnot(
    length(paragraphs) > 0,
    identical(length(images), length(paragraphs)),
    all(file.exists(images)),
    dir.exists(output_dir)
  )

  # Setup objects to populate in for-loop with tts()
  wave_objects <- vector(mode = "list", length = length(paragraphs))
  paragraphs_along <- seq_along(paragraphs)
  ideal_duration <- rep(NA, length(paragraphs))

  # Progress bar
  pb <- progress::progress_bar$new(
    format = " Downloading [:bar] :percent eta: :eta",
    total = 100, clear = TRUE, width = 60)

  # Iterate through arguments used in tts()
  for (ii in paragraphs_along) {
    args <- tts_engine_args
    args$text <- paragraphs[ii]
    args$bind_audio <- TRUE
    wav <- do.call(tts_engine, args = args)
    wav <- reduce(wav$wav, bind)
    wav <- pad_wav(wav, duration = duration[ii])
    ideal_duration[ii] <- length(wav@left) / wav@samp.rate
    wave_objects[[ii]] <- wav
    # Advance progress bar
    pb$tick()
  }
  # Burn subtitles
  if (subtitles) {
    sub_file <- paste0(tools::file_path_sans_ext(output), ".srt")
    ari_subtitles(paragraphs, wave_objects, sub_file)
    message("Subtitle file successfully generated")
  }

  # Create a video from images and audio
  res <- ari_stitch(images, wave_objects, output)
  # Path to output
  output_path <- attr(res, "outfile")

  # Check if larger than 0 bytes
  output_size <- file.info(output_path)$size

  if (output_size > 0) {
    return(output_path)
  } else {
    stop("File does not exist. Something went wrong.")
  }

}

#' @rdname ari_spin
#' @export
have_polly <- function() {
  requireNamespace("aws.polly", quietly = TRUE)
}
