library(testthat)
library(ari)
library(tuneR)
library(purrr)
library(aws.polly)

# aws.signature::use_credentials(profile = "polly")
# set_audio_codec("libfdk_aac")
test_check("ari")
