make_same_sample_rate <- function(audio, verbose = TRUE) {
  if (inherits(audio, "Wave")) {
    return(audio)
  }

  sample_rate <- sapply(audio, function(r) r@samp.rate)
  if (!all(sample_rate == sample_rate[[1]]) && verbose) {
    message("enforcing same sample rate, using minimum")
  }
  sample_rate <- min(sample_rate, na.rm = TRUE)
  if (verbose) {
    message(paste0("Sample rate downsampled to ", sample_rate))
  }
  audio <- lapply(audio, function(x) {
    if (x@samp.rate == sample_rate) {
      return(x)
    }
    tuneR::downsample(x, samp.rate = sample_rate)
  })
  sample_rate <- sapply(audio, function(r) r@samp.rate)
  stopifnot(all(sample_rate == sample_rate[[1]]))
  return(audio)
}

is_Wave <- function(x) {
  identical(suppressWarnings(as.character(class(x))), "Wave")
}

# get random string
grs <- function() {
  paste(sample(c(seq(10), letters, LETTERS),
    size = 12, replace = TRUE
  ), collapse = "")
}

# how long is a wav?
duration <- function(wav) {
  stopifnot(is_Wave(wav))
  length(wav@left) / wav@samp.rate
}

# get from list
# list, name of element, default
gfl <- function(l, n, d) {
  if (is.null(l[[n]])) {
    d
  } else {
    l[[n]]
  }
}

#' @importFrom purrr map_chr compose
string_tirm <- function(s) {
  str_rev <- function(t) {
    paste(rev(strsplit(t, NULL)[[1]]), collapse = "")
  }

  str_trim_right <- function(x) {
    sub("\\s+$", "", x)
  }

  str_trim_left <- function(x) {
    x <- str_rev(x)
    x <- str_trim_right(x)
    str_rev(x)
  }

  lr <- compose(str_trim_left, str_trim_right)
  map_chr(s, lr)
}

# get text from html comments in an Rmd
parse_html_comments <- function(path) {
  lines_ <- readLines(path, warn = FALSE)
  starts <- grep("<!--", lines_)
  ends <- grep("-->", lines_)

  if (length(starts) != length(ends)) {
    stop("There's a comment open/close mismatch.")
  }

  result <- rep(NA, length(starts))

  for (i in seq_along(starts)) {
    if (starts[i] == ends[i]) { # Single line
      result[i] <- lines_[starts[i]]
    } else {
      # Multiple lines
      result[i] <- paste(string_tirm(lines_[starts[i]:ends[i]]),
        collapse = " "
      )
    }
    result[i] <- sub("<!--", "", result[i])
    result[i] <- sub("-->", "", result[i])
  }

  string_tirm(result)
}

# split a big string into equal-ish sized pieces
#' @importFrom purrr map
split_up_text <- function(text) {
  pieces <- ceiling(nchar(text) / 1500)
  words <- strsplit(text, " ")[[1]]
  chunks <- split(words, ceiling(seq_along(words) / (length(words) / pieces)))
  map(chunks, paste, collapse = " ")
}
