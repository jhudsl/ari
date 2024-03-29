% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ari_talk.R
\name{ari_talk}
\alias{ari_talk}
\title{Create spoken audio files}
\usage{
ari_talk(
  paragraphs,
  output = tempfile(fileext = ".wav"),
  voice = text2speech::tts_default_voice(service = service),
  service = "amazon"
)
}
\arguments{
\item{paragraphs}{A vector strings that will be spoken by Amazon Polly.}

\item{output}{A path to the audio file which will be created.}

\item{voice}{The  voice you want to use. See
\code{\link[text2speech]{tts_voices}} for more information
about what voices are available.}

\item{service}{speech synthesis service to use,
passed to \code{\link[text2speech]{tts}}
Either \code{"amazon"} or \code{"google"}.}
}
\value{
A \code{Wave} output object, with the attribute \code{outfile}
of the output file name.
}
\description{
A simple function for demoing how spoken text will sound.
}
