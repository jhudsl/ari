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
#' @importFrom purrr reduce discard
#' @importFrom tuneR bind writeWave
#' @export
#' @examples 
#' \dontrun{
#' 
#' library(tuneR)
#' library(purrr)
#' 
#' slides <- c("intro.jpeg", "equations.jpeg", "questions.jpeg")
#' sound <- map(c("rec1.wav", "rec2.wav", "rec3.wav"), readWave)
#' 
#' ari_stitch(slides, sound, audio_codec = "aac")
#' 
#' }
ari_stitch <- function(
  images, audio, 
  output = "output.mp4",
  verbose = FALSE,
  cleanup = TRUE,
  ffmpeg_opts = "",
  divisible_height = FALSE,
  audio_codec = get_audio_codec(),
  video_codec = get_video_codec(),
  audio_bitrate = "192k",
  video_bitrate = NULL
){
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  stopifnot(
    length(audio) > 0,
    identical(length(images), length(audio)),
    all(file.exists(images)),
    dir.exists(output_dir)
  )
  if (is.character(audio)) {
    audio = lapply(audio, tuneR::readMP3)
    audio = lapply(audio, function(wav) {
      ideal_duration <- ceiling(length(wav@left) / wav@samp.rate)
      left = rep(0, 
                 wav@samp.rate * ideal_duration - length(wav@left))
      right = numeric(0)
      if (wav@stereo) {
        right = left
      }
      end_wav = tuneR::Wave(
        left = left,
        right = right,
        bit = wav@bit, samp.rate = wav@samp.rate)         
      wav <- bind(wav, end_wav)
      wav      
    })
  }
  # Make a hard path
  output = file.path(output_dir, basename(output))
  
  if (verbose > 0) {
    message("Writing out Wav for audio")
  }
  wav <- purrr::reduce(audio, bind)
  wav_path <- file.path(output_dir, paste0("ari_audio_", grs(), ".wav"))
  writeWave(wav, filename = wav_path)
  if (cleanup) {
    on.exit(unlink(wav_path, force = TRUE), add = TRUE)
  }
  
  input_txt_path <- file.path(output_dir, paste0("ari_input_", grs(), ".txt"))
  ## on windows ffmpeg cancats names adding the working directory, so if
  ## complete url is provided it adds it twice.
  # if (.Platform$OS.type == "windows") {
  #   images <- basename(images)   
  # }
  for(i in 1:length(images)){
    cat(paste0("file ", "'", images[i], "'", "\n"), file = input_txt_path, append = TRUE)
    cat(paste0("duration ", duration(audio[[i]]), "\n"), file = input_txt_path, append = TRUE)
  }
  cat(paste0("file ", "'", images[i], "'", "\n"), file = input_txt_path, append = TRUE)
  
  ffmpeg = ffmpeg_exec()
  
  if (divisible_height) {
    ffmpeg_opts = c(ffmpeg_opts, 
                    '-vf "scale=trunc(iw/2)*2:trunc(ih/2)*2"')
  }
  
  ffmpeg_opts = paste(ffmpeg_opts, collapse = " ")
  # shQuote should seankross/ari#5
  command <- paste(
    ffmpeg, "-y -f concat -safe 0 -i", shQuote(input_txt_path), 
    "-i", shQuote(wav_path), 
    ifelse(!is.null(video_codec), paste("-c:v", video_codec),
           ""),
    ifelse(!is.null(audio_codec), paste("-c:a", audio_codec),
           ""),    
    ifelse(!is.null(audio_bitrate), paste("-b:a", audio_bitrate),
           ""), 
    ifelse(!is.null(video_bitrate), paste("-b:v", video_bitrate),
           ""), 
    " -shortest -vsync vfr -pix_fmt yuv420p",
    ffmpeg_opts,
    shQuote(output))
  if (verbose > 0) {
    message(command)
  }
  if (verbose > 1) {
    message("Input text path is:")
    cat(readLines(input_txt_path), sep = "\n")
  }
  res = system(command)
  if (res != 0) {
    warning("Result was non-zero for ffmpeg")
  }
  
  if (cleanup) {
    on.exit(unlink(input_txt_path, force = TRUE), add = TRUE)
  }
  res = file.exists(output) && file.size(output) > 0
  if (!cleanup) {
    attr(res, "txt_path") = input_txt_path
    attr(res, "wav_path") = wav_path
  }
  attr(res, "outfile") = output
  invisible(res)
}
