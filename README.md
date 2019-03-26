# ari

[![Travis-CI Build Status](https://travis-ci.org/seankross/ari.svg?branch=master)](https://travis-ci.org/seankross/ari)
[![CRAN version](http://www.r-pkg.org/badges/version/ari)](https://cran.r-project.org/package=ari)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ari)](http://cran-logs.rstudio.com/)

muschellij2 badge:

[![Travis-CI Build Status](https://travis-ci.org/muschellij2/ari.svg?branch=master)](https://travis-ci.org/muschellij2/ari)

### The Automated R Instructor

Ari is an R package designed to help you make videos from plain text files. Ari
uses [Amazon Polly](https://aws.amazon.com/polly/) to convert your text into
speech. You can then provide images or a set of HTML slides which Ari will
narrate based on a script you provide. Ari uses FFmpeg to stitch together audio
and images.

## Installation

```R
install.packages("ari")
```

You can also install the development version of Ari from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("seankross/ari@dev")
```

You also need to make sure you have [FFmpeg](https://ffmpeg.org/) version
3.2.4 or higher installed on your system.

## Getting Started

First create an HTML slide presentation based in R Markdown using a package
like [`rmarkdown`](https://github.com/rstudio/rmarkdown) or
[`xaringan`](https://github.com/yihui/xaringan). To see an example presentation
enter `browseURL(ari_example("ari_intro.html"))` into the R console. For every
slide in the package you should write some text that will be read while the
slide is being shown. You can do this in a separate Markdown file (see
`file.show(ari_example("ari_intro_script.md"))`) or you can use HTML comments
to put narration right in your `.Rmd` file (see
`file.show(ari_example("ari_comments.Rmd"))`). Make sure to knit your `.Rmd`
file into the HTML slides you want to be turned into a video.

Once you've finished your script and slides install the
[`aws.polly`](https://github.com/cloudyr/aws.polly) package. You can find a
guide for quickly setting up R to use Amazon Web Services
[here](http://seankross.com/2017/05/02/Access-Amazon-Web-Services-in-R.html).
Run `aws.polly::list_voices()` to make sure your keys are working (this
function should return a data frame). Once you've set up your access keys you
can start using Ari.

## Examples

These examples make use of the `ari_example()` function. In order to view the
files mentioned here you should use `file.show(ari_example("[file name]"))`.
You can watch an example of a video produced by Ari
[here](https://youtu.be/dcIUu4GCOKU).

```R
library(ari)

# First set up your AWS keys
Sys.setenv("AWS_ACCESS_KEY_ID" = "EA6TDV7ASDE9TL2WI6RJ",
           "AWS_SECRET_ACCESS_KEY" = "OSnwITbMzcAwvHfYDEmk10khb3g82j04Wj8Va4AA",
           "AWS_DEFAULT_REGION" = "us-east-2")

# Create a video from a Markdown file and slides
ari_narrate(
  ari_example("ari_intro_script.md"),
  ari_example("ari_intro.html"),
  voice = "Joey")

# Create a video from an R Markdown file with comments and slides
ari_narrate(
  ari_example("ari_comments.Rmd"),
  ari_example("ari_intro.html"),
  voice = "Kendra")

# Create a video from images and strings
ari_spin(
  ari_example(c("mab1.png", "mab2.png")),
  c("This is a graph.", "This is another graph"),
  voice = "Joanna")

# Create a video from images and Waves
library(tuneR)
ari_stitch(
  ari_example(c("mab1.png", "mab2.png")),
  list(noise(), noise()))
```

### RMarkdown/HTML slide Problems

Some html slides take a bit to render on webshot, and can be dark gray instead of white.  If you change the `delay` argument in `ari_narrate`, passed to `webshot`, this can resolve some issues, but may take a bit longer to run.  Also, using `capture_method = "vectorized"` is faster, but may have some issues, so run with `capture_method = "iterative"` if this is the case as so:

```r
ari_narrate(
  ari_example("ari_comments.Rmd"),
  ari_example("ari_intro.html"),
  voice = "Kendra",
  delay = 0.5,
  capture_method = "iterative")
```
## Why Use Ari?

Creating videos from plain text has some significant advantages:

1. Video content can be version controlled with Git and GitHub - after all it's
just plain text!
2. Videos with explicit narration are more accessible to students. We don't have
to rely on YouTube's often faulty captioning algorithm.
3. Scripts can be automatically translated into other languages with services
like the [Google Translation API](https://cloud.google.com/translate/docs/) and
Amazon Polly can speak languages other than English. This means you can write
a lecture once and generate slides and videos in multiple languages.

At the [Johns Hopkins Data Science Lab](http://jhudatascience.org/) we rapidly
develop highly technical content about the latest libraries and technologies
available to data scientists. Video production requires a significant time
investment and APIs are always changing. If the interface to a software library
changes it's particularly arduous to re-record an entire lecture because some
function arguments changed. By using Ari we hope to be able to rapidly create
and update video content.
