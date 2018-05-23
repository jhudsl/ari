#' Set Default Audio and Video Codecs
#'
#' @param codec The codec to use or get for audio/video.  Uses the
#' \code{ffmpeg_audio_codec} and \code{ffmpeg_video_codec} options
#' to store this information.
#' @seealso \code{\link{ffmpeg_codecs}} for options
#' @return NULL
#'
#' @examples
#' get_audio_codec()
#' set_audio_codec(codec = "libfdk_aac")
#' get_audio_codec()
#' set_audio_codec(codec = "aac")
#' get_audio_codec()
#' 
#' get_video_codec()
#' set_video_codec(codec = "libx265") 
#' get_video_codec()
#' set_video_codec(codec = "libx264")
#' get_video_codec()
#' @rdname codecs
#' @export
set_audio_codec = function(codec = "aac") {
  options(ffmpeg_audio_codec = codec)
}

#' @export
#' @rdname codecs
set_video_codec = function(codec = "libx264") {
  options(ffmpeg_video_codec = codec)
}

#' @export
#' @rdname codecs
get_audio_codec = function() {
  codec = getOption("ffmpeg_audio_codec")
  if (is.null(codec)) {
    codec = "aac"
  }
  return(codec)
  options(ffmpeg_audio_codec = codec)
}

#' @export
#' @rdname codecs
get_video_codec = function() {
  codec = getOption("ffmpeg_video_codec")
  if (is.null(codec)) {
    codec = "libx264"
  }
  return(codec)
}


#' @rdname codecs
#' @export
#' @examples
#' audio_codec_encode("aac")
audio_codec_encode = function(codec) {
  res = ffmpeg_audio_codecs()
  stopifnot(length(codec) == 1)
  res = res[ res$codec %in% codec, ]
  res$encoding_supported
}

#' @rdname codecs
#' @export
#' @examples
#' video_codec_encode("libx264")
video_codec_encode = function(codec) {
  res = ffmpeg_video_codecs()
  stopifnot(length(codec) == 1)
  res = res[ res$codec %in% codec, ]
  res$encoding_supported
}
