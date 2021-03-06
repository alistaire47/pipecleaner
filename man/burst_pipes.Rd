% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/debug_pipeline.R
\name{burst_pipes}
\alias{burst_pipes}
\title{Replace pipes with intermediate assignment}
\usage{
burst_pipes(pipeline, names, ...)
}
\arguments{
\item{pipeline}{A pipeline. Can be an unquoted or quoted expression, or a
character vector of code. If missing, uses the text highlighted in
RStudio's source editor.}

\item{names}{A character vector of names to be used for intermediary
assignment of a length equal to that of the number of top-level calls in
the pipeline.}

\item{...}{Only used to allow assignment with \code{=} at the beginning of a
pipeline.}
}
\value{
A list of expressions of the restructured, unpiped code, invisibly.
}
\description{
\code{burst_pipes} rearranges a magrittr pipeline into equivalent unpiped code.
Called directly, it will print the restructured code to the console. Called
via the "Burst pipes" RStudio add-in while a pipeline is highlighted, it
will replace the highlighted code with the restructured code. The "Burst
pipes and set names" add-in opens a Shiny gadget in which names can be set.
}
\details{
Note that nested pipelines are currently ignored. Calling on pipelines from
the inside out should still allow restructuring.

Warning: Calling \code{burst_pipes} from the RStudio add-in is currently
irreversible aside from "Edit > Undo" and version control. Save code
beforehand and check equivalence afterwards.
}
\examples{
\donttest{
library(magrittr)

burst_pipes(x <- 1:5 \%>\% rev \%>\% {. * 2} \%>\% .[3] \%>\% rnorm(1, ., sd = ./10))

burst_pipes(
    x = 1:5 \%>\% rev \%>\% {. * 2} \%>\% .[3] \%>\% rnorm(1, ., sd = ./10),
    names = c("reversed", "doubled", "third", "x")
)
}

}
\seealso{
\code{\link{split_pipeline}} splits a pipeline without restructuring.
}
