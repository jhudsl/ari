library(methods)
library(testthat)
library(ari)
library(tuneR)
library(purrr)
library(aws.polly)

if (nzchar(Sys.getenv("AWS_ACCESS_KEY_ID"))) {
  if (!identical(Sys.getenv("TRAVIS"), "true")) {
      aws.signature::use_credentials(profile = "polly")
  }
}
skip_amazon_not_authorized = function() {
  if (text2speech::tts_amazon_authenticated()) {
    return(invisible(TRUE))
  }
  skip("Amazon not authenticated()")
}

test_check("ari")
