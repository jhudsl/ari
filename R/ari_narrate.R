#' Create a video from slides and a script
#' 
#' Given a script and slides, make a video.
#'
#' @param script A markdown file where every paragraph will be read over a
#' corresponding slide.
#' @param slides A URL for an HTML slideshow created with \code{rmarkdown},
#' \code{xaringan}, or a similar package.
#' @param output A path to the video file which will be created.
#' @param voice The Amazon Polly voice you want to use. See \code{list_voices}.
#' @param ws_args Arguments to be passed to \code{webshot}.
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom rmarkdown render html_document
#' @importFrom purrr map_chr walk
#' @importFrom webshot webshot
#' @importFrom aws.polly list_voices
#' @export
ari_narrate <- function(script, slides, output = "output.mp4", voice, 
                        ws_args = list()){
  if(length(list_voices()) < 1){
    stop("It appears you're not connected to Amazon Polly. Make sure you've", 
         "set the appropriate environmental variables before you proceed.")
  }
  
  output_dir <- normalizePath(dirname(output))
  stopifnot(
    file.exists(script),
    dir.exists(output_dir)
  )
  
  html_path <- file.path(output_dir, paste0("ari_script_", grs(), ".html"))
  render(script, output_format = html_document(), output_file = html_path)
  paragraphs <- map_chr(html_text(html_nodes(read_html(html_path), "p")), 
                        function(x){gsub("\u2019", "'", x)})
  unlink(html_path, force = TRUE)
  
  img_paths <- rep(NA, length(paragraphs))
  
  for(i in 1:length(paragraphs)){
    img_paths[i] <- file.path(output_dir, paste0("ari_img_", i, "_", grs(), ".jpeg"))
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
  
  ari_spin(img_paths, paragraphs, output, voice)
  walk(img_paths, unlink, force = TRUE)
  invisible()
}