context("Test comment parsing")

test_that("Comments can be parsed correctly.", {
  script <- system.file("test", "ari_comments.Rmd", package = "ari")
  parsed <- ari:::parse_html_comments(script)
  reference <- c("Hello and welcome to this short presentation about Ari, the automated R instructor.", 
                 "Ari is an R package which allows you to create lecture videos from plain text. Your lectures are therefore easy to edit, translate, and they are universally accessible.", 
                 "You can install Ari using the dev tools package.",
                 "Ari is just a bundling of existing technologies including, r markdown and Polly, a text to speech product from Amazon Web Services.", 
                 "Thank you for watching this video and good luck using Ari!")
  expect_equal(parsed, reference)
})

