#' Convert Files using FFMPEG
#'
#' @param file Video/PNG file to convert
#' @param outfile output file
#' @param overwrite should output file be overwritten?
#' @param args arguments to pass to \code{\link{system2}} to pass to 
#' \code{ffmpeg}
#'
#' @return A character string of the output file with different attributes
#' @export
#'
#' @examples
#' pngfile = tempfile(fileext = ".png")
#' png(pngfile)
#' plot(0, 0)
#' dev.off()
#' if (have_ffmpeg_exec()) {
#' res = ffmpeg_convert(pngfile)
#' }
ffmpeg_convert = function(
  file, 
  outfile = tempfile(
    fileext = paste0(".", tools::file_ext(file))
  ),
  overwrite = TRUE,
  args = NULL
) {
  ffmpeg = ffmpeg_exec(quote = FALSE)
  file = normalizePath(file)
  add_y = NULL
  if (file.exists(outfile)) {
    if (!overwrite) {
      stop("outfile already exists, overwrite = FALSE")
    } else {
      add_y = "-y"
    }
  }
  args = c(add_y, "-i", file, args, outfile)
  suppressWarnings({
    res = system2(ffmpeg, args = args, stdout = TRUE, stderr = TRUE)
  })
  attr(outfile, "result") = res
  return(outfile)
}