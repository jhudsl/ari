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
