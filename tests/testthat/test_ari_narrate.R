context("Test ari_narrate()")


if (ffmpeg_version_sufficient()) {
  res = ffmpeg_audio_codecs()
  if (is.null(res)) {
    fdk_enabled = FALSE
  } else {
    fdk_enabled = grepl("fdk", res[ res$codec == "aac", "codec_name"])
  }  
} else {
  fdk_enabled = FALSE
}
if (fdk_enabled) {
  audio_codec = "libfdk_aac"
} else {
  audio_codec = "ac3"
}

skip_narrate <- function(){
  if (Sys.getenv("SKIP_NARRATE") != "") {
    skip("skipping ari_narrate()")
  }
}

video <- file.path(tempdir(), "output.mp4")
#video <- file.path(getwd(), "output.mp4")

run_voice = "Joanna"

test_that("Ari can make a video from local HTML slides.", {
  skip_on_cran()
  skip_narrate()
  skip_amazon_not_authorized()

  ari_narrate(system.file("test", "ari_intro_script.md", package = "ari"),
              system.file("test", "ari_intro.html", package = "ari"),
              video, voice = run_voice,
              capture_method = "iterative",
              service = "amazon",
              verbose = TRUE,
              audio_codec = audio_codec)
  expect_true(file.size(video) > 50000)
})

unlink(video, force = TRUE)

test_that("Ari can make a video from HTML slides on the web.", {
  skip_on_cran()
  skip_narrate()
  skip_amazon_not_authorized()

  ari_narrate(system.file("test", "ari_intro_script.md", package = "ari"),
              "https://seankross.com/ari/inst/test/ari_intro.html",
              video, voice = run_voice, 
              capture_method = "iterative",
              verbose = TRUE,
              audio_codec = audio_codec)
  expect_true(file.size(video) > 50000)
})

unlink(video, force = TRUE)

test_that("Ari can use an Rmd file with HTML comments for a script.", {
  skip_on_cran()
  skip_narrate()
  skip_amazon_not_authorized()

  ari_narrate(system.file("test", "ari_comments.Rmd", package = "ari"),
              system.file("test", "ari_intro.html", package = "ari"),
              video, voice = run_voice, capture_method = "iterative",
              verbose = TRUE,
              audio_codec = audio_codec)
  expect_true(file.size(video) > 50000)
})

unlink(video, force = TRUE)
