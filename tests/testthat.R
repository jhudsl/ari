library(methods)
library(testthat)
library(ari)
library(tuneR)
library(purrr)
library(aws.polly)

aws.signature::use_credentials(profile = "polly")
test_check("ari")
