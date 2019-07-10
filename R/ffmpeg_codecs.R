#' Get Codecs for ffmpeg
#'
#' @return A `data.frame` of codec names and capabilities
#' @export
#'
#' @examples
#' if (ffmpeg_version_sufficient()) {
#' ffmpeg_codecs()
#' ffmpeg_audio_codecs()
#' ffmpeg_video_codecs()
#' }
ffmpeg_codecs = function() {
  ffmpeg = ffmpeg_exec()
  cmd = paste(ffmpeg, "-codecs")
  res = system(cmd, intern = TRUE, ignore.stderr = TRUE)
  res = trimws(res)
  res = res[grepl("^([.]|D)", res)]
  res = strsplit(res, " ")
  res = t(vapply(res, function(x) {
    x = trimws(x)
    x = x[ x != ""]
    if (length(x) >= 3) {
      x[3:length(x)] = paste(x[3:length(x)], collapse = " ")
    }
    return(x[seq(3)])
  }, FUN.VALUE = character(3)))
  colnames(res) = c("capabilities", "codec", "codec_name")
  res = as.data.frame(res, stringsAsFactors = FALSE)
  res$capabilities = trimws(res$capabilities)
  
  cap_defns = res[ res$codec == "=", ]
  res = res[ res$codec != "=", ]
  
  cap = do.call("rbind", strsplit(res$capabilities, split = ""))
  
  cap_defns$codec_name = tolower(cap_defns$codec_name)
  cap_defns$codec_name = gsub(" ", "_", cap_defns$codec_name)
  cap_defns$codec_name = gsub("-", "_", cap_defns$codec_name)
  cap_def = do.call("rbind", strsplit(cap_defns$capabilities, split = ""))
  
  mat = matrix(NA, ncol = nrow(cap_defns), nrow = nrow(cap))
  colnames(mat) = cap_defns$codec_name
  
  icol = 4
  indices = apply(cap_def, 1, function(x) which(x != "."))
  for (icol in seq(nrow(cap_def))) {
    x = cap[, indices[icol]]
    mat[, icol] = x %in% cap_def[icol, indices[icol]]
  }
  mat = as.data.frame(mat, stringsAsFactors = FALSE)
  
  res = cbind(res, mat)
  if (any(rowSums(
    res[, c("video_codec", "audio_codec", "subtitle_codec")])
    > 1)) {
    warning("Format may have changed, please post this issue")
  }
  
  # L = list(capabilities = cap_defns,
  #          codecs = res)
  # return(L)
  return(res)
}

#' @rdname ffmpeg_codecs
#' @export
ffmpeg_video_codecs = function() {
  res = ffmpeg_codecs()
  res = res[ res$video_codec, ]
  res$video_codec = res$audio_codec = res$subtitle_codec = NULL
  res
}

#' @rdname ffmpeg_codecs
#' @export
ffmpeg_audio_codecs = function() {
  res = ffmpeg_codecs()
  res = res[ res$audio_codec, ]
  res$video_codec = res$audio_codec = res$subtitle_codec = NULL
  res
}



#' @rdname ffmpeg_codecs
#' @export
ffmpeg_muxers = function() {
  ffmpeg = ffmpeg_exec()
  cmd = paste(ffmpeg, "-muxers")
  res = system(cmd, intern = TRUE, ignore.stderr = TRUE)
  res = trimws(res)
  res = res[grepl("^E", res)]
  res = strsplit(res, " ")
  res = t(vapply(res, function(x) {
    x = trimws(x)
    x = x[ x != ""]
    if (length(x) >= 3) {
      x[3:length(x)] = paste(x[3:length(x)], collapse = " ")
    }
    return(x[seq(3)])
  }, FUN.VALUE = character(3)))
  colnames(res) = c("capabilities", "muxer", "muxer_name")
  res = as.data.frame(res, stringsAsFactors = FALSE)
  res$capabilities = trimws(res$capabilities)
  
  return(res)
}

#' @rdname ffmpeg_codecs
#' @export
ffmpeg_version = function() {
  ffmpeg = ffmpeg_exec()
  cmd = paste(ffmpeg, "-version")
  res = system(cmd, intern = TRUE, ignore.stderr = TRUE)
  res = trimws(res)
  res = res[grepl("^ffmpeg version", res)]
  res = sub("ffmpeg version (.*) Copyright .*", "\\1", res)
  res = trimws(res)
  return(res)
}

#' @rdname ffmpeg_codecs
#' @export
ffmpeg_version_sufficient = function() {
  if (have_ffmpeg_exec()) {
    ver = package_version("3.2.4")
    ff_ver = package_version(ffmpeg_version())
    res = ff_ver >= ver
  } else {
    res = FALSE
  }
  res
}

#' @rdname ffmpeg_codecs
#' @export
check_ffmpeg_version = function() {
  if (!ffmpeg_version_sufficient()) {
    ff = ffmpeg_version()
    stop(paste0(
      "ffmpeg version is not high enough,", 
      " ffmpeg version is: ", ff))
  }
  return(invisible(NULL))
}