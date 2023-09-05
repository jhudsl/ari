#' Burn Subtitles into a video
#'
#' @note This needs \code{ffmpeg} that was compiled with
#' \code{--enable-libass} as per
#' \url{https://trac.ffmpeg.org/wiki/HowToBurnSubtitlesIntoVideo}
#'
#' @param input_video Path to video in \code{mp4} format
#' @param srt Path to subtitle file in \code{srt} format
#' @param output_video Path to video with subtitles
#' @param verbose print diagnostic messages.  If > 1,
#' then more are printed
#'
#' @return Name of output video
#' @export
ari_burn_subtitles <- function(input_video, srt,
                               output_video = tempfile(fileext = ".mp4"),
                               verbose = FALSE) {
  ffmpeg <- ffmpeg_exec(quote = TRUE)
  if (verbose > 0) {
    message("Burning in Subtitles")
  }
  command <- paste(
    ffmpeg, "-y -i", input_video, paste0("-vf subtitles=", srt),
    output_video
  )

  if (verbose > 0) {
    message(command)
  }
  res <- system(command)
  if (res != 0) {
    warning("Result was non-zero for ffmpeg")
  }

  output_video
}
