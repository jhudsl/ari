#' List of arguments to the Coqui text-to-speech engine
#'
#' @param model_name Deep Learning model for Text-to-Speech Conversion
#' @param vocoder_name Voice coder used for speech coding and transmission
#'
#' @return List of arguments
#' @export
#'
#' @examples
#' coqui_args(model_name = "tacotron2-DDC_ph", vocoder_name = "ljspeech/univnet")
coqui_args <- function(model_name = "jenny", vocoder_name = "jenny") {
list(service = "coqui",
     voice = NULL,
     model_name = model_name,
     vocoder_name = vocoder_name,
     output_format = "wav")
}
