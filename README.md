
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipecleaner

[![Travis build
status](https://travis-ci.org/alistaire47/pipecleaner.svg?branch=master)](https://travis-ci.org/alistaire47/pipecleaner)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/agx2ht7cpwrnrk0k?svg=true)](https://ci.appveyor.com/project/alistaire47/pipecleaner)
[![Coverage
status](https://codecov.io/gh/alistaire47/pipecleaner/branch/master/graph/badge.svg)](https://codecov.io/github/alistaire47/pipecleaner?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/pipecleaner)](https://cran.r-project.org/package=pipecleaner)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

pipecleaner is a utility R package to debug pipelines using the magrittr
`%>%` pipe. Its `debug_pipeline` launches the debugging browser on the
input pipeline in a form that allows the user to step through the
successive calls of the pipeline, examining the output of each
successive element.

## Installation

pipecleaner is not currently on CRAN, but can be installed with

``` r
# install.packages("devtools")
devtools::install_github("alistaire47/pipecleaner")
```

## Debugging pipelines

To debug a pipeline, call `debug_pipeline` on the raw code or a
character vector of code. If no input is supplied and it is called from
[RStudio](https://www.rstudio.com/products/RStudio/), it will use
whatever code is highlighed in the source editor as input.

`debug_pipeline` can also be called via an [RStudio
add-in](https://rstudio.github.io/rstudioaddins/) by highlighting the
pipeline to debug and then selecting “Debug pipeline in browser” from
the “Addins” menu.

Once called, `debug_pipeline` will reassemble the pipeline into a
function that can be debugged in the browser and call the debugger. Each
line adds another call from the pipeline and prints and the output so
the user can see the status of the data passed through the pipeline by
stepping through the function.

The data is also stored to a variable called `dot[N]` in each line,
where `[N]` is the index of the call, making it easy to compare input
and output data of a step in the pipeline and try out new code
formulations in the console.

All together, it looks like this:

``` r
library(magrittr)
library(pipecleaner)

debug_pipeline(
    x = 1:5 %>% 
        rev %>% 
        {. * 2} %>% 
        sample(replace = TRUE)
)
#> dot1 <- rev(1:5)
#> dot2 <- {dot1 * 2}
#> x <- sample(dot2, replace = TRUE)debugging in: pipeline_function()
#> debug: {
#>     print(dot1 <- rev(1:5))
#>     print(dot2 <- {
#>         dot1 * 2
#>     })
#>     print(x <- sample(dot2, replace = TRUE))
#> }
#> debug at /Users/alistaire/Documents/R_projects/pipecleaner/R/debug_pipeline.R#270: print(dot1 <- rev(1:5))
#> [1] 5 4 3 2 1
#> debug: print(dot2 <- {
#>     dot1 * 2
#> })
#> debug: dot1 * 2
#> [1] 10  8  6  4  2
#> debug: print(x <- sample(dot2, replace = TRUE))
#> [1] 2 8 4 2 6
#> exiting from: pipeline_function()
```

## Bursting pipes

Occasionally it is necessary to restructure code from a piped to an
unpiped form. @hrbrmstr dubbed this process “pipe bursting”:

<blockquote class="twitter-tweet" data-partner="tweetdeck">

<p lang="en" dir="ltr">

coined two new phrases whilst making that r pkg script thing:<br><br>-
“declawing” == removing purrr as a dependency<br>- “pipe bursting” ==
moving from clean %\>% to sequential \<- statements

</p>

— boB Rudis (@hrbrmstr)
<a href="https://twitter.com/hrbrmstr/status/1019700751268970502?ref_src=twsrc%5Etfw">July
18,
2018</a>

</blockquote>

<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

Now `burst_pipes` makes this sort of restructuring simple:

``` r
burst_pipes(
    x = 1:5 %>% 
        rev %>% 
        {. * 2} %>% 
        .[3] %>% 
        rnorm(1, ., sd = ./10)
  )
#> dot1 <- rev(1:5)
#> dot2 <- {dot1 * 2}
#> dot3 <- dot2[3]
#> x <- rnorm(1, dot3, sd = dot3/10)
```

More specific names can be specified as a character vector:

``` r
burst_pipes(
    x <- 1:5 %>% 
        rev %>% 
        {. * 2} %>% 
        .[3] %>% 
        rnorm(1, ., sd = ./10),
    names = c("reversed", "doubled", "subset", "x")
)
#> reversed <- rev(1:5)
#> doubled <- {reversed * 2}
#> subset <- doubled[3]
#> x <- rnorm(1, subset, sd = subset/10)
```

`burst_pipes` can also be called via an RStudio add-in, in which case it
will replace the highlighted code with its restructured form.

## Limitations

pipecleaner should successfully debug most pipelines. However, due to
its structure, it does have known limitations:

  - It can only handle the `%>%` pipe, not more exotic pipes like `%$%`.
    For the moment, this is unlikely to change absent significant
    demand.
  - It ignores nested pipelines—e.g. piping within an anonymous function
    in `purrr::map`—treating the whole call as one step.

## Related

  - [ViewPipeSteps](https://github.com/daranzolin/ViewPipeSteps) is a
    similar project that calls `View()` after each step in the pipeline.
  - magrittr itself contains `debug_pipe`, which is a wrapper around
    `browser` that returns its input, allowing it to be inserted within
    a pipeline.
