#' Get the path to an ari example file
#'
#' This function allows you to quickly access files that are used in
#' the ari documentation.
#'
#' @param path The name of the file. If no argument is provided then
#' all of the example files will be listed.
#' 
#' @return A character string
#' @export
#' @examples
#' ari_example("ari_intro.Rmd")
ari_example <- function(path = NULL){
  if(is.null(path)) {
    list.files(system.file("test", package = "ari"))
  } else {
    system.file("test", path, package = "ari", mustWork = TRUE)
  }
}