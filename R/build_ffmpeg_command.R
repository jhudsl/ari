# Build ffmpeg command to input into system()
build_ffmpeg_command <- function(input_txt_path, wav_path, ffmpeg_args, output) {
  # Path to ffmpeg
  ffmpeg <- ffmpeg_exec(quote = TRUE)

  # Frames per second (fps)
  if (!is.null(ffmpeg_args$frames_per_second)) {
    video_filters <- c(ffmpeg_args$video_filters, paste0("fps=", ffmpeg_args$frames_per_second))
  } else {
    video_filters <- c(ffmpeg_args$video_filters, "fps=5")
  }

  # Divisible height
  if (ffmpeg_args$divisible_height) {
    video_filters <- c(video_filters, '"scale=trunc(iw/2)*2:trunc(ih/2)*2"')
  }

  # workaround for older ffmpeg
  # https://stackoverflow.com/questions/32931685/
  # the-encoder-aac-is-experimental-but-experimental-codecs-are-not-enabled
  experimental <- FALSE
  if (!is.null(ffmpeg_args$audio_codec)) {
    if (ffmpeg_args$audio_codec == "aac") {
      experimental <- TRUE
    }
  }
  if (ffmpeg_args$deinterlace) {
    video_filters <- c(video_filters, "yadif")
  }
  video_filters <- paste(video_filters, collapse = ",")
  video_filters <- paste0("-vf ", video_filters)

  if (any(grepl("-vf", ffmpeg_args$deinterlace))) {
    warning("Found video filters in ffmpeg_opts, may not be used correctly!")
  }
  ffmpeg_opts <- c(video_filters, ffmpeg_args$ffmpeg_opts)
  ffmpeg_opts <- paste(ffmpeg_opts, collapse = " ")

  # ffmpeg command
  command <- paste(
    ffmpeg, "-y",
    "-f concat -safe 0 -i", shQuote(input_txt_path),
    "-i", shQuote(wav_path),
    ifelse(!is.null(ffmpeg_args$video_codec), paste("-c:v", ffmpeg_args$video_codec),
           ""
    ),
    ifelse(!is.null(ffmpeg_args$audio_codec), paste("-c:a", ffmpeg_args$audio_codec),
           ""
    ),
    ifelse(ffmpeg_args$stereo_audio, "-ac 2", ""),
    ifelse(!is.null(ffmpeg_args$audio_bitrate), paste("-b:a", ffmpeg_args$audio_bitrate),
           ""
    ),
    ifelse(!is.null(ffmpeg_args$video_bitrate), paste("-b:v", ffmpeg_args$video_bitrate),
           ""
    ),
    # ifelse(deinterlace, "-vf yadif", ""),
    ifelse(!is.null(ffmpeg_args$video_sync_method), paste("-fps_mode", "auto"),
           ""
    ),
    ifelse(!is.null(ffmpeg_args$pixel_format), paste("-pix_fmt", ffmpeg_args$pixel_format),
           ""
    ),
    ifelse(ffmpeg_args$fast_start, "-movflags +faststart", ""),
    ffmpeg_opts,
    ifelse(!is.null(ffmpeg_args$frames_per_second), paste0("-r ", ffmpeg_args$frames_per_second), ""),
    ifelse(experimental, "-strict experimental", ""),
    "-max_muxing_queue_size 9999",
    "-threads 2",
    shQuote(output)
  )

  command
}

# Default ffmpeg arguments
set_ffmpeg_args <- function() {
  list(frames_per_second = NULL,
       video_filters = NULL,
       divisible_height = TRUE,
       audio_codec = get_audio_codec(),
       video_codec = get_video_codec(),
       deinterlace = FALSE,
       ffmpeg_opts = "",
       audio_bitrate = NULL,
       video_bitrate = NULL,
       video_sync_method = "2",
       pixel_format = "yuv420p",
       fast_start = FALSE,
       stereo_audio = TRUE)
}
