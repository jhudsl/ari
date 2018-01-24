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
#' ari_stitch(slides, sound)
#' 
#' }
ari_stitch <- function(images, audio, 
                       output = "output.mp4",
                       verbose = FALSE,
                       cleanup = TRUE){
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  stopifnot(
    length(audio) > 0,
    identical(length(images), length(audio)),
    all(file.exists(images)),
    dir.exists(output_dir)
  )
  
  if (verbose > 0) {
    message("Writing out Wav for audio")
  }
  wav <- reduce(audio, bind)
  wav_path <- file.path(output_dir, paste0("ari_audio_", grs(), ".wav"))
  writeWave(wav, filename = wav_path)
  if (cleanup) {
    on.exit(unlink(wav_path, force = TRUE), add = TRUE)
  }
  
  input_txt_path <- file.path(output_dir, paste0("ari_input_", grs(), ".txt"))
  for(i in 1:length(images)){
    cat(paste0("file ", "'", images[i], "'", "\n"), file = input_txt_path, append = TRUE)
    cat(paste0("duration ", duration(audio[[i]]), "\n"), file = input_txt_path, append = TRUE)
  }
  cat(paste0("file ", "'", images[i], "'", "\n"), file = input_txt_path, append = TRUE)
  
  ffmpeg <- discard(c(Sys.getenv("ffmpeg"), Sys.which("ffmpeg")), ~ nchar(.x) == 0)[1]
  
  if (is.na(ffmpeg)){
    stop("Could not find ffmpeg. See the documentation for ari_stitch() for more details.")
  }
  
  command <- paste(ffmpeg, "-y -f concat -safe 0 -i", input_txt_path, "-i", 
                   wav_path, "-c:v libx264 -c:a aac -b:a 192k -shortest -vsync vfr -pix_fmt yuv420p",
                   output)
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
  invisible(file.exists(output) && file.size(output) > 0)
}
