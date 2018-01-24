#' Create spoken audio files
#' 
#' A simple function for demoing how spoken text from
#' \href{https://aws.amazon.com/polly/}{Amazon Polly} will sound.
#' @param paragraphs A vector strings that will be spoken by Amazon Polly.
#' @param output A path to the audio file which will be created.
#' @param voice The Amazon Polly voice you want to use. See 
#' \code{\link[aws.polly]{list_voices}} for more information about what voices
#' are available.
#' @importFrom aws.polly list_voices synthesize
#' @importFrom tuneR bind Wave writeWave
#' @importFrom purrr map reduce
#' @export
ari_talk <- function(paragraphs, output = "output.wav", voice) {
  if(length(list_voices()) < 1){
    stop("It appears you're not connected to Amazon Polly. Make sure you've ", 
         "set the appropriate environmental variables before you proceed.")
  }
  output_dir <- normalizePath(dirname(output))
  stopifnot(
    length(paragraphs) > 0,
    dir.exists(output_dir)
  )
  
  wavs <- vector(mode = "list", length = length(paragraphs))
  par_along <- 1:length(paragraphs)
  
  for(i in par_along) {
    if(nchar(paragraphs[i]) < 1500){
      wav <- synthesize(paragraphs[i], voice)
    } else {
      chunks <- split_up_text(paragraphs[i])
      wav <- reduce(map(chunks, synthesize, voice = voice), bind)
    }
    wavs[[i]] <- wav
  }
  
  audio <- reduce(wavs, bind)
  writeWave(audio, output)
}