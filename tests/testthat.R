library(methods)
library(testthat)
library(ari)
library(tuneR)
library(purrr)

skip_amazon_not_authorized = function() {
  if(!identical(Sys.getenv("NOT_CRAN"), "true")) {
    skip("Amazon not authenticated()")
  }
  
  if (text2speech::tts_amazon_authenticated()) {
    return(invisible(TRUE))
  }
  
  skip("Amazon not authenticated()")
}

test_check("ari")
