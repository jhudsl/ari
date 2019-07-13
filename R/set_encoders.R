get_os = function() {
  sys_info = Sys.info()
  os = tolower(sys_info[["sysname"]])
  return(os)
}

#' Set Default Audio and Video Codecs
#'
#' @param codec The codec to use or get for audio/video.  Uses the
#' `ffmpeg_audio_codec` and `ffmpeg_video_codec` options
#' to store this information.
#' @seealso [ffmpeg_codecs()] for options
#' @return A `NULL` output
#'
#' 
#' @rdname codecs
#' @export
#' 
#' @examples
#' 
#' if (have_ffmpeg_exec()) {
#' print(ffmpeg_version())
#' get_audio_codec()
#' set_audio_codec(codec = "libfdk_aac")
#' get_audio_codec()
#' set_audio_codec(codec = "aac")
#' get_audio_codec()
#' }
#' if (have_ffmpeg_exec()) {
#' get_video_codec()
#' set_video_codec(codec = "libx265") 
#' get_video_codec()
#' set_video_codec(codec = "libx264")
#' get_video_codec()
#' }
#' ## empty thing
#' if (have_ffmpeg_exec()) {
#' video_codec_encode("libx264")
#' 
#' audio_codec_encode("aac")
#' }
#' 
set_audio_codec = function(codec) {
  if (missing(codec)) {
    os = get_os()
    codec = switch(os,
                   darwin = "libfdk_aac",
                   windows = "ac3",
                   linux = "aac"
    )
  }
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
    os = get_os()
    res = ffmpeg_audio_codecs()
    if (is.null(res)) {
      fdk_enabled = FALSE
    } else {
      fdk_enabled = grepl("fdk", res[ res$codec == "aac", "codec_name"])
    }
    if (fdk_enabled) {
      os_audio_codec = "libfdk_aac"
    } else {
      os_audio_codec = "aac"
    }
    codec = switch(os,
                   darwin = os_audio_codec,
                   windows = "ac3",
                   linux = "aac"
    )
    set_audio_codec(codec = codec)
  }    
  return(codec)
}

#' @export
#' @rdname codecs
get_video_codec = function() {
  codec = getOption("ffmpeg_video_codec")
  if (is.null(codec)) {
    codec = "libx264"
    set_video_codec(codec = codec)
  }
  return(codec)
}


#' @rdname codecs
#' @export
audio_codec_encode = function(codec) {
  res = ffmpeg_audio_codecs()
  if (is.null(res)) {
    warning("Codec could not be checked")
    return(NA)
  }  
  stopifnot(length(codec) == 1)
  res = res[ res$codec %in% codec | 
               grepl(codec, res$codec_name), ]
  res$encoding_supported
}

#' @rdname codecs
#' @export
video_codec_encode = function(codec) {
  res = ffmpeg_video_codecs()
  if (is.null(res)) {
    warning("Codec could not be checked")
    return(NA)
  }  
  stopifnot(length(codec) == 1)
  res = res[ res$codec %in% codec | 
               grepl(codec, res$codec_name), ]
  res$encoding_supported
}


