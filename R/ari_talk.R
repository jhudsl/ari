#' Create spoken audio files
#' 
#' A simple function for demoing how spoken text will sound.
#' @param paragraphs A vector strings that will be spoken by Amazon Polly.
#' @param output A path to the audio file which will be created.
#' @param voice The  voice you want to use. See 
#' \code{\link[text2speech]{tts_voices}} for more information 
#' about what voices are available.
#' @param service speech synthesis service to use,
#' passed to \code{\link[text2speech]{tts}}
#' 
#' @return A \code{Wave} output object, with the attribute \code{outfile}
#' of the output file name.
#' @importFrom text2speech tts_auth tts
#' @importFrom tuneR bind Wave writeWave
#' @importFrom purrr map reduce
#' @export
ari_talk <- function(paragraphs, 
                     output = tempfile(fileext = ".wav"),
                     voice = text2speech::tts_default_voice(service = service),
                     service = ifelse(have_polly(), "amazon", "google")) {
  auth = text2speech::tts_auth(service = service)
  if (!auth) {
    stop(paste0("It appears you're not authenticated with ", 
                service, ". Make sure you've ", 
                "set the appropriate environmental variables ", 
                "before you proceed.")
    )
  }
  output_dir <- normalizePath(dirname(output))
  stopifnot(
    length(paragraphs) > 0,
    dir.exists(output_dir)
  )
  
  wavs <- vector(mode = "list", length = length(paragraphs))
  par_along <- seq_along(paragraphs)
  
  for (i in par_along) {
    wav <- text2speech::tts(
      text = paragraphs[i], 
      voice = voice,
      service = service,
      bind_audio = TRUE)
    wav = reduce(wav$wav, bind)
    wavs[[i]] <- wav
  }
  
  audio <- reduce(wavs, bind)
  writeWave(audio, output)
  attr(audio, "outfile") = output
  return(audio)
}