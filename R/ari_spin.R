#' Ari Spin
#' 
#' Given images and text, make a video
#' 
#' @param images A vector of paths to images.
#' @param paragraphs A vector strings that will be spoken by Amazon Polly.
#' @param output A path to the video file which will be created.
#' @param voice The Amazon Polly voice you want to use. See \code{list_voices}.
#' @importFrom aws.polly list_voices synthesize
#' @importFrom tuneR bind Wave
#' @export
ari_spin <- function(images, paragraphs, output = "output.mp4", voice){
  if(length(list_voices()) < 1){
    stop("It appears you're not connected to Amazon Polly. Make sure you've", 
         "set the appropriate environmental variables before you proceed.")
  }
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  stopifnot(
    length(paragraphs) > 0,
    identical(length(images), length(paragraphs)),
    all(file.exists(images)),
    dir.exists(output_dir)
  )
  
  wavs <- list()
  par_along <- 1:length(paragraphs)
  ideal_duration <- rep(NA, length(paragraphs))
  
  for(i in par_along){
    wav <- synthesize(paragraphs[i], voice)
    ideal_duration[i] <- ceiling(length(wav@left) / wav@samp.rate)
    end_wav <- Wave(rep(0, wav@samp.rate * ideal_duration[i] - length(wav@left)),
                    bit = wav@bit, samp.rate = wav@samp.rate)
    wavs[[i]] <- bind(wav, end_wav)
  }
  
  ari_stitch(images, wavs, output)
}