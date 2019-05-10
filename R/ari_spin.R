#' Create a video from images and text
#' 
#' Given equal length vectors of paths to images (preferably \code{.jpg}s
#' or \code{.png}s) and strings which will be
#' \code{\link[aws.polly]{synthesize}}d by
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
#' to Amazon Polly see the \code{\link[aws.polly]{aws.polly}} documentation 
#' \href{https://github.com/cloudyr/aws.polly}{here}.
#' 
#' @param images A vector of paths to images.
#' @param paragraphs A vector strings that will be spoken by Amazon Polly.
#' @param output A path to the video file which will be created.
#' @param voice The voice you want to use. See 
#' \code{\link[text2speech]{tts_voices}} for more information 
#' about what voices are available.
#' @param service speech synthesis service to use,
#' passed to \code{\link[text2speech]{tts}}
#' @param subtitles Should a \code{.srt} file be created with subtitles? The
#' default value is \code{FALSE}. If \code{TRUE} then a file with the same name
#' as the \code{output} argument will be created, but with the file extension
#' \code{.srt}.
#' @param ... additional arguments to \code{\link{ari_stitch}}
#' 
#' @importFrom text2speech tts_auth tts
#' @importFrom tuneR bind Wave
#' @importFrom purrr map reduce
#' @importFrom progress progress_bar
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples 
#' \dontrun{
#' 
#' slides <- system.file("test", c("mab2.png", "mab1.png"),
#' package = "ari")
#' sentences <- c("Welome to my very interesting lecture.",
#'                "Here are some fantastic equations I came up with.")
#' ari_spin(slides, sentences, voice = "Joey")
#' 
#' }
#' 
ari_spin <- function(
  images, paragraphs, 
  output = tempfile(fileext = ".mp4"),
  voice = "Joanna",
  service = "amazon",
  subtitles = FALSE,
  ...){
  # check for ffmpeg before any synthesizing
  ffmpeg_exec()
  
  auth = text2speech::tts_auth(service = service)
  if (!auth) {
    stop(paste0("It appears you're not authenticated with ", 
                service, ". Make sure you've ", 
                "set the appropriate environmental variables ", 
                "before you proceed.")
    )
  }
  
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  
  if (length(paragraphs) == 1) {
    if (file.exists(paragraphs)) {
      paragraphs = readLines(paragraphs, warn = FALSE)
      paragraphs = paragraphs[ !paragraphs %in% "" ]
    }
  }
  
  stopifnot(
    length(paragraphs) > 0,
    identical(length(images), length(paragraphs)),
    all(file.exists(images)),
    dir.exists(output_dir)
  )
  
  wavs <- vector(mode = "list", length = length(paragraphs))
  par_along <- 1:length(paragraphs)
  ideal_duration <- rep(NA, length(paragraphs))
  
  pb <- progress_bar$new(
    format = "Fetching Narration [:bar] :percent", 
    total = length(par_along))
  
  for (i in par_along) {
    wav <- text2speech::tts(
      text = paragraphs[i], 
      voice = voice,
      service = service,
      bind_audio = TRUE)
    wav = reduce(wav$wav, bind)
    ideal_duration[i] <- ceiling(length(wav@left) / wav@samp.rate)
    end_wav <- Wave(
      rep(0, wav@samp.rate * ideal_duration[i] - length(wav@left)),
      bit = wav@bit, samp.rate = wav@samp.rate)
    wavs[[i]] <- bind(wav, end_wav)
    pb$tick()
  }
  
  if (subtitles) {
    sub_file = paste0(file_path_sans_ext(output), ".srt")
    ari_subtitles(paragraphs, wavs, sub_file)
  }
  
  
  res = ari_stitch(images, wavs, output, ...)
  args = list(...)
  cleanup = args$cleanup
  if (is.null(cleanup)) {
    cleanup = TRUE
  }
  if (!cleanup) {
    attr(res, "wavs") = wavs
  }
  return(res)
}