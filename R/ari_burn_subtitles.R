#' Burn Subtitles into a video
#' 
#' @note This needs \code{ffmpeg} that was compiled with 
#' \code{--enable-libass} as per 
#' \url{https://trac.ffmpeg.org/wiki/HowToBurnSubtitlesIntoVideo}
#'
#' @param video Video in \code{mp4} format
#' @param srt Subtitle file in \code{srt} format
#' @param verbose print diagnostic messages.  If > 1, 
#' then more are printed
#'
#' @return Name of output video
ari_burn_subtitles = function(video, srt, verbose = FALSE) {
  ffmpeg <- discard(c(Sys.getenv("ffmpeg"), 
                      Sys.which("ffmpeg")), ~ nchar(.x) == 0)[1]
  
  if (is.na(ffmpeg)) {
    stop("Could not find ffmpeg. See the documentation for ari_stitch() for more details.")
  }
  if (verbose > 0) {
    message("Burning in Subtitles")
  }
  command <- paste(
    ffmpeg, "-y -i", video, paste0("-vf subtitles=", srt),
    video)

  if (verbose > 0) {
    message(command)
  }
  res = system(command)
  if (res != 0) {
    warning("Result was non-zero for ffmpeg")
  }
  
  return(video)
}