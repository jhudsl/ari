# Checks if it has a uniform sample rate across all channels.
# If the sample rates are not uniform, the function downsamples
# the audio to the minimum sample rate.
match_sample_rate <- function(audio, verbose = TRUE) {
  if (inherits(audio, "Wave")) {
    return(audio)
  }
  # Extract sample rates of all channels of audio w aveform
  sample_rate <- sapply(audio, function(r) r@samp.rate)

  # if sample rates across all channels are not uniform, and
  # verbose is TRUE, print message
  if (!all(sample_rate == sample_rate[[1]]) && verbose) {
    message("enforcing same sample rate, using minimum")
  }
  # Compute minimum sample rate across all channels
  sample_rate <- min(sample_rate, na.rm = TRUE)
  if (verbose) {
    message(paste0("Sample rate downsampled to ", sample_rate))
  }
  # Downsample audio waveform to min sample rate if sample rate
  # is not equal to minimum sample rate
  audio <- lapply(audio, function(x) {
    if (x@samp.rate == sample_rate) {
      return(x)
    }
    tuneR::downsample(x, samp.rate = sample_rate)
  })
  # Extract new sample rates
  sample_rate <- sapply(audio, function(r) r@samp.rate)
  # Check if all channels  of downsampled audio waveform have same sample rate
  stopifnot(all(sample_rate == sample_rate[[1]]))

  # Return downsampled audio waveform
  return(audio)
}

# Check if x is class "Wave"
is_Wave <- function(x) {
  identical(suppressWarnings(as.character(class(x))), "Wave")
}

# Get random string
get_random_string <- function() {
  paste(sample(c(seq(10), letters, LETTERS),
    size = 12, replace = TRUE
  ), collapse = "")
}

# Find length of WAV file
wav_length <- function(wav) {
  stopifnot(is_Wave(wav))
  length(wav@left) / wav@samp.rate
}

# Extract from list
# Parameters: list, name of element, default
extract_list <- function(l, n, d) {
  if (is.null(l[[n]])) {
    d
  } else {
    l[[n]]
  }
}

# Trim whitespace from the beginning and end of each string in a character vector
#' @importFrom purrr map_chr compose
string_trim <- function(s) {
  # Reverse a string
  str_rev <- function(t) {
    paste(rev(strsplit(t, NULL)[[1]]), collapse = "")
  }

  # Remove any whitespace from end of string
  str_trim_right <- function(x) {
    sub("\\s+$", "", x)
  }
  # Remove any whitespace from beginning of string
  str_trim_left <- function(x) {
    x <- str_rev(x)
    x <- str_trim_right(x)
    str_rev(x)
  }
  # Create new function `lr` that applies
  # str_trim_left and str_trim_right sequentially to a given string
  lr <- purrr::compose(str_trim_left, str_trim_right)
  # Apply the lr function to each element of the input vectors
  purrr::map_chr(s, lr)
}

# Get text from html comments in an Rmd
parse_html_comments <- function(path) {
  lines_ <- readLines(path, warn = FALSE)
  # Identify where the HTML comment starts and ends
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
      result[i] <- paste(string_trim(lines_[starts[i]:ends[i]]),
        collapse = " "
      )
    }
    # Remove opening "<!--" and closing "-->" markers from comment
    result[i] <- sub("<!--", "", result[i])
    result[i] <- sub("-->", "", result[i])
  }

  string_trim(result)
}

# Split a big string into equal-ish sized pieces
#' @importFrom purrr map
split_up_text <- function(text) {
  # Calculate number of piceces to split up text
  pieces <- ceiling(nchar(text) / 1500)
  # Split up vector into individual words
  words <- strsplit(text, " ")[[1]]
  # Split words into pieces number of chunks
  chunks <- split(words, ceiling(seq_along(words) / (length(words) / pieces)))

  # Final output
  map(chunks, paste, collapse = " ")
}

# Returns type of operating system
os_type <- function() {
  .Platform$OS.type
}

# Determine type of operating system
sys_type <- function() {
  if (os_type() == "windows") {
    "windows"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "macos"
  } else if (Sys.info()["sysname"] == "Linux") {
    "linux"
  } else if (os_type() == "unix") {
    # "unix"
    "linux"
  } else {
    stop("Unknown OS")
  }
}

# Sets LD_LIBRARY_PATH environment variable to path of
# LibreOffice program on Linux or macOS systems if the variable is not already set
fix_soffice_library_path = function() {
  LD_LIBRARY_PATH = Sys.getenv("LD_LIBRARY_PATH")
  if (sys_type() %in% c("linux", "macos")) {
    warning(
      paste0(
        "Changing LD_LIBRARY_PATH as error in soffice ",
        "with PPTX conversion may be due to path issues!"
      )
    )
    Sys.setenv(
      LD_LIBRARY_PATH =
        paste0(
          "/usr/lib/libreoffice/program",
          if (nzchar(LD_LIBRARY_PATH)) paste0(":", LD_LIBRARY_PATH)
        )
    )
  }
}
