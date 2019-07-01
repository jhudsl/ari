#' Get Path to ffmpeg Executable
#'
#' @return The path to the \code{ffmpeg} executable, or an error.
#' @export
#'
#' @examples
#' ffmpeg_exec()
ffmpeg_exec = function() {
  ffmpeg <- discard(c(Sys.getenv("ffmpeg"), 
                      Sys.which("ffmpeg")), ~ nchar(.x) == 0)[1]
  
  if (is.na(ffmpeg)) {
    stop(paste("Could not find ffmpeg. See the documentation ", 
               "for ari_stitch() ", 
               "for more details."))
  }
  return(ffmpeg)
}

#' @export
#' @rdname ffmpeg_exec
have_ffmpeg_exec = function() {
  exec = try({
    ari::ffmpeg_exec()
  }, silent = TRUE)
  !inherits(exec, "try-error")
}