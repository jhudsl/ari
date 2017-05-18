context("Test ari_narrate()")

skip_narrate <- function(){
  if(Sys.getenv("SKIP_NARRATE") != ""){
    skip("skipping ari_narrate()")
  }
}

video <- file.path(tempdir(), "output.mp4")

test_that("Ari can make a video from local HTML slides.", {
  skip_on_cran()
  skip_on_travis()
  skip_narrate()
  
  ari_narrate(system.file("test", "ari_intro_script.md", package = "ari"),
              system.file("test", "ari_intro.html", package = "ari"),
              video, voice = list_voices()$Id[1])
  expect_true(file.size(video) > 50000)
})

unlink(video, force = TRUE)

test_that("Ari can make a video from HTML slides on the web.", {
  skip_on_cran()
  skip_narrate()
  
  ari_narrate(system.file("test", "ari_intro_script.md", package = "ari"),
              "http://seankross.com/ari/inst/test/ari_intro.html",
              video, voice = list_voices()$Id[1])
  expect_true(file.size(video) > 50000)
})

unlink(video, force = TRUE)

test_that("Ari use an Rmd file with HTML comments for a script.", {
  skip_on_cran()
  skip_narrate()
  
  ari_narrate(system.file("test", "ari_comments.Rmd", package = "ari"),
              system.file("test", "ari_intro.html", package = "ari"),
              video, voice = list_voices()$Id[1])
  expect_true(file.size(video) > 50000)
})

unlink(video, force = TRUE)
