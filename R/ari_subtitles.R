# paragraphs - strings of text
# dutations - paragraph duration in seconds
# path - path to .srt file output
#' @importFrom purrr map map_dbl
#' @importFrom hms hms
ari_subtitles <- function(paragraphs, wavs, path) {
  durations <- map_dbl(wavs, ~ length(.x@left) / .x@samp.rate)
  result <- seq_along(paragraphs)
  
  cumdur <- cumsum(durations)
  cumdur <- map(cumdur, hms)
  cumdur <- map(cumdur, as.character)
  cumdur <- map(cumdur, substr, start = 0, stop = 12)
  cumdur <- map(cumdur, gsub, pattern = "\\.", replacement = ",")
  cumdur <- c("00:00:00,000", unlist(cumdur))
  
  result <- map(result, ~ c(.x, paste(cumdur[.x], "-->", cumdur[.x + 1]), 
                            strwrap(paragraphs[.x], width = 60), ""))
  result <- unlist(result)
  writeLines(result, path)
}