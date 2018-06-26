library(methods)
library(testthat)
library(ari)
library(tuneR)
library(purrr)
library(aws.polly)

if (!nzchar(Sys.getenv("AWS_ACCESS_KEY_ID"))) {
  aws.signature::use_credentials(profile = "polly")
}
test_check("ari")
