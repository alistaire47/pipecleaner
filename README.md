
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipecleaner

[![Travis build
status](https://travis-ci.org/alistaire47/pipecleaner.svg?branch=master)](https://travis-ci.org/alistaire47/pipecleaner)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/alistaire47/pipecleaner?branch=master&svg=true)](https://ci.appveyor.com/project/alistaire47/pipecleaner)
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
devtools::install_github("pipecleaner")
```

## Debugging pipelines

To debug a pipeline, call `debug_pipeline` on the raw code or a
character string of the code. If no input is supplied and it is called
from [RStudio](https://www.rstudio.com/products/RStudio/), it will use
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
    1:5 %>% 
        rev() %>% 
        {. * 2} %>% 
        sample()
)
#> debugging in: pipeline_function()
#> debug: {
#>     print(dot1 <- 1:5)
#>     print(dot2 <- 1:5 %>% rev())
#>     print(dot3 <- 1:5 %>% rev() %>% {
#>         . * 2
#>     })
#>     print(dot4 <- 1:5 %>% rev() %>% {
#>         . * 2
#>     } %>% sample())
#> }
#> debug: print(dot1 <- 1:5)
#> [1] 1 2 3 4 5
#> debug: print(dot2 <- 1:5 %>% rev())
#> [1] 5 4 3 2 1
#> debug: print(dot3 <- 1:5 %>% rev() %>% {
#>     . * 2
#> })
#> [1] 10  8  6  4  2
#> debug: print(dot4 <- 1:5 %>% rev() %>% {
#>     . * 2
#> } %>% sample())
#> [1]  2  8  6  4 10
#> exiting from: pipeline_function()
```

## Limitations

pipecleaner should successfully debug most pipelines. However, due to
its structure, it does have known limitations:

  - It can only handle the `%>%` pipe, not more exotic pipes like `%$%`.
    For the moment, this is unlikely to change, as accommodating varying
    pipes would require a more complicated approach.
  - It cannot handle nested pipelines, e.g. piping within
    `dplyr::mutate`. This may be rectified eventually.

## Related

  - [ViewPipeSteps](https://github.com/daranzolin/ViewPipeSteps) is a
    similar project that calls `View()` after each step in the pipeline.
  - magrittr itself contains `debug_pipe`, which is a wrapper around
    `browser` that returns its input, allowing it to be inserted within
    a pipeline.
