#' Pad Wave Objects
#'
#' @param wav list of Wave objects
#' @param duration If \code{NULL}, the duration will simply round
#' the Wave up to the next whole integer.  If not, these are the
#' duration to pad the Wave *to*. For example 12 means the output
#' Wave will have a length of 12 seconds.  Pass \code{NA} to those
#' Waves that you want simple rounding.
#'
#' @return A list of Wave objects, same length as input \code{wav}
#' @export
#'
#' @importFrom purrr map2 map2_int
#' @examples
#' wavs <- list(
#'   tuneR::noise(duration = 1.85 * 44100),
#'   tuneR::noise()
#' )
#' out <- pad_wav(wavs)
#' dur <- sapply(out, function(x) length(x@left) / x@samp.rate)
#' duration <- c(2, 2)
#' out <- pad_wav(wavs, duration = duration)
#' dur <- sapply(out, function(x) length(x@left) / x@samp.rate)
#' stopifnot(all(dur == duration))
#' duration <- c(2, 2.5)
#' out <- pad_wav(wavs, duration = duration)
#' dur <- sapply(out, function(x) length(x@left) / x@samp.rate)
#' stopifnot(isTRUE(all.equal(dur, duration)))
pad_wav <- function(wav, duration = NULL) {
  # See if wav inherits from "Wave" class
  is_Wave <- inherits(wav, "Wave")
  if (is_Wave) {
    wav <- list(wav)
  }
  if (is.null(duration)) {
    duration <- rep(NA, length(wav))
  }
  stopifnot(length(duration) == length(wav))
  # Iterate over wav and find "ideal duration"
  duration <- purrr::map2_int(.x = wav, .y = duration,
                       .f = function(wav, dur) {
                         ideal_duration <- ceiling(length(wav@left) / wav@samp.rate)
                         if (!is.na(dur)) {
                           ideal_duration <- max(ideal_duration, dur)
                         }
                         ideal_duration
                       })
  # Iterate over wav and create end_wav that binds to existing wav
  out_wav <- purrr::map2(.x = wav,
                  .y = duration,
                  .f = function(wav, ideal_duration) {
                    left <- rep(0, wav@samp.rate * ideal_duration - length(wav@left))
                    right <- numeric(0)
                    if (wav@stereo) {
                      right <- left
                    }
                    end_wav <- tuneR::Wave(
                      left = left,
                      right = right,
                      bit = wav@bit,
                      samp.rate = wav@samp.rate,
                      pcm = wav@pcm
                    )
                    wav <- tuneR::bind(wav, end_wav)
                    wav
                  })

  if (is_Wave) {
    out_wav <- out_wav[[1]]
  }

  return(out_wav)
}
