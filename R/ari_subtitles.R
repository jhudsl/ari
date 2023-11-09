#' Generate subtitle files for audio for video content
#'
#' @param paragraphs String of text
#' @param wavs Wave objects from tuneR
#' @param path Path to .srt file output
#' @param width Width of each subtitle
#'
#' @importFrom purrr map_dbl
#' @importFrom hms hms
#' @export
#' @examples
#' \dontrun{
#' # Audio files (.wav)
#' audio <- system.file("test", c("audio1.wav", "audio2.wav"), package = "ari")
#'
#' # Import audio files as wave objects
#' first_wav_object <- tuneR::readWave(audio[1])
#' second_wav_object <- tuneR::readWave(audio[2])
#' # Put wave objects inside a list
#' wavs <- list(first_wav_object, second_wav_object)
#'
#' # Text
#' sentences <- c(
#'   "Welcome to my very interesting lecture.",
#'   "Here are some fantastic equations I came up with."
#' )
#'
#' ari_subtitles(sentences, wavs, "output.srt")
#' }
ari_subtitles <- function(paragraphs, wavs, path, width = 42) {
  # Calculate the duration of each audio file
  durations <- map_dbl(wavs, ~ length(.x@left) / .x@samp.rate)

  # Break down paragraphs so that they are more readable
  paragraphs <- map(paragraphs, strwrap, width = width)
  # Find number of lines in each paragraph
  lines_per_paragraph <- map_dbl(paragraphs, length)
  durations <- rep(durations / lines_per_paragraph, times = lines_per_paragraph)
  paragraphs <- unlist(paragraphs)

  # Convert cumulative duration to format hh:mm:ss,ms
  cumdur <- cumsum(durations)
  cumdur <- map(cumdur, hms::hms)
  cumdur <- map(cumdur, as.character)
  cumdur <- map(cumdur, substr, start = 0, stop = 12)
  cumdur <- map(cumdur, gsub, pattern = "\\.", replacement = ",")
  # Check if there is a comma separating seconds and milliseconds, add one if necessary
  cumdur <- unlist(cumdur)
  add <- !grepl(",", cumdur)
  cumdur[add] <- paste0(cumdur[add], ",000")

  # Add a starting cumulative duration of 00:00:00,000
  cumdur <- c("00:00:00,000", unlist(cumdur))
  # Split the cumulative durations into seconds and milliseconds
  cumdur <- strsplit(cumdur, ",")
  # Format the milliseconds to have three digits (e.g. 023 instead of 23)
  cumdur <- map_chr(cumdur, function(x) {
    x[2] <- sprintf("%03.0f", as.numeric(x[2]))
    x <- paste(x, collapse = ",")
  })

  # Combine paragraphs, cumulative durations, and other subtitle formatting
  result <- seq_along(paragraphs)
  result <- map(result, ~ c(
    .x, paste(cumdur[.x], "-->", cumdur[.x + 1]),
    paragraphs[.x], ""
  ))
  result <- unlist(result)

  # Write the resulting subtitle strings to a file at the specified path
  writeLines(result, path)
}
