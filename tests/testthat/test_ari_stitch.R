context("Test ari_stitch()")

res = ffmpeg_audio_codecs()
fdk_enabled = grepl("fdk", res[ res$codec == "aac", "codec_name"])
if (fdk_enabled) {
  audio_codec = "libfdk_aac"
} else {
  audio_codec = "aac"
}

test_that("ari_stitch() can combine audio and images into a video", {
  skip_on_cran()
  # should work without polly
  temp_dir <- tempdir()
  
  for (i in 1:3) {
    jpeg(file.path(temp_dir, paste0("plot", i, ".jpg")))
    plot(1:5 * i, 1:5, main = i)
    dev.off()
  }
  
  sound <- replicate(
    3, 
    tuneR::Wave(round(rnorm(88200, 127, 20)), 
                samp.rate = 44100, bit = 16))
  
  graphs <- file.path(temp_dir, paste0("plot", 1:3, ".jpg"))
  video <- file.path(temp_dir, "output.mp4")
  
  on.exit(walk(c(graphs, video), unlink, force = TRUE), add = TRUE)
  
  ari_stitch(graphs, sound, output = video,
             audio_codec = audio_codec)
  
  expect_true(file.size(video) > 50000)
})
