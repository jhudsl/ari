#' Create a video from slides and a script
#' 
#' \code{ari_narrate} creates a video from a script written in markdown and HTML
#' slides created with \code{\link[rmarkdown]{rmarkdown}} or a similar package.
#' This function uses \href{https://aws.amazon.com/polly/}{Amazon Polly} 
#' via \code{\link{ari_spin}}.
#'
#' @param script Either a markdown file where every paragraph will be read over
#' a corresponding slide, or an \code{.Rmd} file where each HTML comment will
#' be used for narration.
#' @param slides A path or URL for an HTML slideshow created with 
#' \code{\link[rmarkdown]{rmarkdown}}, \code{xaringan}, or a 
#' similar package.
#' @param output The path to the video file which will be created.
#' @param voice The Amazon Polly voice you want to use. See
#' \code{\link[aws.polly]{list_voices}}.
#' @param capture_method Either \code{"vectorized"} or \code{"iterative"}.
#' The vectorized mode is faster though it can cause screens to repeat. If
#' making a video from an \code{\link[rmarkdown]{ioslides_presentation}}
#' you should use \code{"iterative"}.
#' @param subtitles Should a \code{.srt} file be created with subtitles? The
#' default value is \code{FALSE}. If \code{TRUE} then a file with the same name
#' as the \code{output} argument will be created, but with the file extension
#' \code{.srt}.
#' @param ... Arguments that will be passed to \code{\link[webshot]{webshot}}.
#' @param verbose print diagnostic messages.  If > 1, then more are printed
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom rmarkdown render html_document
#' @importFrom purrr map_chr walk
#' @importFrom webshot webshot
#' @importFrom aws.polly list_voices
#' @importFrom tools file_ext
#' @export
#' @examples 
#' \dontrun{
#' 
#' # 
#' ari_narrate(system.file("test", "ari_intro_script.md", package = "ari"),
#'             system.file("test", "ari_intro.html", package = "ari"),
#'             voice = "Joey")
#' 
#' }
ari_narrate <- function(script, slides, output = "output.mp4", voice,
                        capture_method = "vectorized",
                        subtitles = FALSE, ...,
                        verbose = FALSE){
  if(length(list_voices()) < 1){
    stop("It appears you're not connected to Amazon Polly. Make sure you've", 
         "set the appropriate environmental variables before you proceed.")
  }
  
  if(!(capture_method %in% c("vectorized", "iterative"))) {
    stop('capture_method must be either "vectorized" or "iterative"')
  }
  
  output_dir <- normalizePath(dirname(output))
  script <- normalizePath(script)
  if(file.exists(slides)){
    slides <- normalizePath(slides)
    slides <- paste0("file://localhost", slides)
  }
  stopifnot(
    file.exists(script),
    dir.exists(output_dir)
  )
  
  if(file_ext(script) %in% c("Rmd", "rmd")){
    paragraphs <- parse_html_comments(script)
  } else {
    html_path <- file.path(output_dir, paste0("ari_script_", grs(), ".html"))
    on.exit(unlink(html_path, force = TRUE), add = TRUE)
    render(script, output_format = html_document(), output_file = html_path)
    paragraphs <- map_chr(html_text(html_nodes(read_html(html_path), "p")), 
                          function(x){gsub("\u2019", "'", x)})
  }
  
  slide_nums <- 1:length(paragraphs)
  img_paths <- file.path(output_dir, paste0("ari_img_", slide_nums, "_", grs(), ".jpeg"))
  
  if(capture_method == "vectorized"){
    webshot(url = paste0(slides, "#", slide_nums), file = img_paths, ...)
  } else {
    for(i in slide_nums){
      webshot(url = paste0(slides, "#", i), file = img_paths[i], ...)
    }
  }
  
  on.exit(walk(img_paths, unlink, force = TRUE), add = TRUE)
  ari_spin(img_paths, paragraphs, output, voice, subtitles, 
           verbose = verbose)
  invisible()
}