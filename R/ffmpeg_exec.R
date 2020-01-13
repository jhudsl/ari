#' Get Path to ffmpeg Executable
#'
#' @return The path to the \code{ffmpeg} executable, or an error.
#' @note This looks using `Sys.getenv("ffmpeg")` and `Sys.which("ffmpeg")`
#' to find `ffmpeg`.  If `ffmpeg` is not in your PATH, then please set the
#' path to `ffmpeg` using `Sys.setenv(ffmpeg = "/path/to/ffmpeg")`
#' @param quote should \code{\link{shQuote}} be run before returning?
#' @export
#'
#' @examples
#' \dontrun{
#' if (have_ffmpeg_exec()) { 
#' ffmpeg_exec()
#' }
#' }
ffmpeg_exec = function(quote = FALSE) {
  ffmpeg <- discard(c(Sys.getenv("ffmpeg"), 
                      Sys.which("ffmpeg")), ~ nchar(.x) == 0)[1]
  
  if (is.na(ffmpeg)) {
    stop(paste("Could not find ffmpeg. See the documentation ", 
               "for ari_stitch() ", 
               "for more details."))
  }
  if (!ffmpeg %in% c("ffmpeg", "ffmpeg.exe")) {
    ffmpeg = normalizePath(ffmpeg, winslash = "/")
  }
  if (quote) {
    ffmpeg = shQuote(ffmpeg)
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