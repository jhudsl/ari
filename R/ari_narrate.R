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
#' @param ws_args Arguments to be passed to \code{\link[webshot]{webshot}}.
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
                        ws_args = list()){
  if(length(list_voices()) < 1){
    stop("It appears you're not connected to Amazon Polly. Make sure you've", 
         "set the appropriate environmental variables before you proceed.")
  }
  
  output_dir <- normalizePath(dirname(output))
  script <- normalizePath(script)
  if(file.exists(slides)){
    slides <- normalizePath(slides)
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
  
  img_paths <- rep(NA, length(paragraphs))
  
  for(i in 1:length(paragraphs)){
    img_paths[i] <- file.path(output_dir, paste0("ari_img_", i, "_", grs(), ".jpeg"))
    
    if(file.exists(slides)){
      slides <- paste0("file://localhost", slides)
    }
    
    webshot(url = paste0(slides, "#", i), file = img_paths[i],
            vwidth = gfl(ws_args, "vwidth", 992),
            vheight = gfl(ws_args, "vheight", 744),
            cliprect = gfl(ws_args, "cliprect", NULL),
            selector = gfl(ws_args, "selector", NULL),
            expand = gfl(ws_args, "expand", NULL),
            delay = gfl(ws_args, "delay", 0.2),
            zoom = gfl(ws_args, "zoom", 1),
            eval = gfl(ws_args, "eval", NULL))
  }
  
  on.exit(walk(img_paths, unlink, force = TRUE), add = TRUE)
  ari_spin(img_paths, paragraphs, output, voice)
  invisible()
}