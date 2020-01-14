# ari 0.3.3

- Added vignette for using via a Docker image.

# ari 0.3.2

- Fixed bug in `ffmpeg_version_sufficient` if `ffmpeg` is a development version.
- Added attributes to `ari_spin` for voice and such.
- Fixed `ffmpeg_version` for versions with a `+` for CRAN.

# ari 0.3.0

- Added in `text2speech` package for use of other audio generation engines.
- Added `have_ffmpeg_exec` to check if the user has `ffmpeg` installed
- Example for `ari_stitch` now is run.
- Some documentation changes for `BiocCheck` compliance (just more rigid).
- Added `ffmpeg_version` and `ffmpeg_version_sufficient` to check `ffmpeg`.

# ari 0.2.0

- Second CRAN release, to be used with `didactr` package.
- Added check functionality like `ffmpeg_exec` to find the ffmpeg executable.
- Added `divisible_height` argument to `ari_stitch` to allow fixes for https://github.com/seankross/ari/issues/9.
- Fixes seankross/ari#5 with the `shQuote` function.


# ari 0.1.0

- First release.
- `ari_stitch()`: create a video from images and audio files.
- `ari_spin()`: create a video from images and text.
- `ari_narrate()`: create a video from an R Markdown file.