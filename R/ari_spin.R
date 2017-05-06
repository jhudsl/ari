#' Create a video from images and text
#' 
#' Given equal length vectors of paths to images (preferably \code{.jpg}s
#' or \code{.png}s) and strings which will be
#' \code{\link[aws.polly]{synthesize}}d by
#' \href{https://aws.amazon.com/polly/}{Amazon Polly}, this function creates an
#' \code{.mp4} video file where each image is shown with
#' its corresponding narration.
#' 
#' This function needs to connect to
#' \href{https://aws.amazon.com/}{Amazon Web Services} in order to create the
#' narration. You can find a guide for accessing AWS from R
#' \href{http://seankross.com/}{here}. For more information about how R connects
#' to Amazon Polly see the \code{\link[aws.polly]{aws.polly}} documentation 
#' \href{https://github.com/cloudyr/aws.polly}{here}.
#' 
#' @param images A vector of paths to images.
#' @param paragraphs A vector strings that will be spoken by Amazon Polly.
#' @param output A path to the video file which will be created.
#' @param voice The Amazon Polly voice you want to use. See 
#' \code{\link[aws.polly]{list_voices}} for more information about what voices
#' are available.
#' @importFrom aws.polly list_voices synthesize
#' @importFrom tuneR bind Wave
#' @importFrom purrr map reduce
#' @export
#' @examples 
#' \dontrun{
#' 
#' slides <- c("intro.jpeg", "equations.jpeg", "questions.jpeg")
#' sentences <- c("Welome to my very interestig lecture.",
#'                "Here are some fantastic equations I came up with.",
#'                "Any questions?")
#' ari_spin(slides, sentences, voice = "Joey")
#' 
#' }
#' 
ari_spin <- function(images, paragraphs, output = "output.mp4", voice){
  if(length(list_voices()) < 1){
    stop("It appears you're not connected to Amazon Polly. Make sure you've ", 
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
    if(nchar(paragraphs[i]) < 1500){
      wav <- synthesize(paragraphs[i], voice)
    } else {
      chunks <- split_up_text(paragraphs[i])
      wav <- reduce(map(chunks, synthesize, voice = voice), bind)
    }
    ideal_duration[i] <- ceiling(length(wav@left) / wav@samp.rate)
    end_wav <- Wave(rep(0, wav@samp.rate * ideal_duration[i] - length(wav@left)),
                    bit = wav@bit, samp.rate = wav@samp.rate)
    wavs[[i]] <- bind(wav, end_wav)
  }
  
  ari_stitch(images, wavs, output)
}