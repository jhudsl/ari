# paragraphs - strings of text
# dutations - paragraph duration in seconds
# path - path to .srt file output
#' @importFrom purrr map map_dbl map2
#' @importFrom hms hms
ari_subtitles <- function(paragraphs, wavs, path) {
  durations <- map_dbl(wavs, ~ length(.x@left) / .x@samp.rate)
  
  # Break down paragraphs so that they are more readable
  paragraphs <- map(paragraphs, strwrap, width = 60)
  lines_per_paragraph <- map_dbl(paragraphs, length)
  durations <- rep(durations / lines_per_paragraph, times = lines_per_paragraph)
  paragraphs <- unlist(paragraphs)
  
  cumdur <- cumsum(durations)
  cumdur <- map(cumdur, hms)
  cumdur <- map(cumdur, as.character)
  cumdur <- map(cumdur, substr, start = 0, stop = 12)
  cumdur <- map(cumdur, gsub, pattern = "\\.", replacement = ",")
  # need to check to see if no , so 00:00:27 doesn't happen
  cumdur = unlist(cumdur)
  add = !grepl(",", cumdur)
  cumdur[ add ] = paste0(cumdur[add], ",0")
  cumdur <- c("00:00:00,000", unlist(cumdur))
  
  result <- seq_along(paragraphs)
  result <- map(result, ~ c(.x, paste(cumdur[.x], "-->", cumdur[.x + 1]), 
                            paragraphs[.x], ""))
  result <- unlist(result)
  writeLines(result, path)
}