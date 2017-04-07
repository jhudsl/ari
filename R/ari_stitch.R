#' Ari Stitch
#' 
#' Given images and wavs, make a video
#' 
#' @param images A vector of paths to images.
#' @param audio A list of \code{Wave}s from tuneR.
#' @param output A path to the video file which will be created.
#' @importFrom purrr reduce
#' @importFrom tuneR bind writeWave
#' @export
ari_stitch <- function(images, audio, output = "output.mp4"){
  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
  stopifnot(
    length(audio) > 0,
    identical(length(images), length(audio)),
    all(file.exists(images)),
    dir.exists(output_dir)
  )
  
  wav <- reduce(audio, bind)
  wav_path <- file.path(output_dir, paste0("ari_audio_", grs(), ".wav"))
  writeWave(wav, filename = wav_path)
  
  input_txt_path <- file.path(output_dir, paste0("ari_input_", grs(), ".txt"))
  for(i in 1:length(images)){
    cat(paste0("file ", "'", images[i], "'", "\n"), file = input_txt_path, append = TRUE)
    cat(paste0("duration ", duration(audio[[i]]), "\n"), file = input_txt_path, append = TRUE)
  }
  cat(paste0("file ", "'", images[i], "'", "\n"), file = input_txt_path, append = TRUE)
  
  command <- paste("ffmpeg -y -f concat -safe 0 -i", input_txt_path, "-i", 
                   wav_path, "-c:v libx264 -c:a aac -b:a 192k -shortest -vsync vfr -pix_fmt yuv420p",
                   output)
  system(command)
  
  unlink(wav_path, force = TRUE)
  unlink(input_txt_path, force = TRUE)
  invisible(file.exists(output) && file.size(output) > 0)
}
