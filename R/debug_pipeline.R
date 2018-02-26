#' Run a pipeline step-by-step in the debugging browser
#'
#' `debug_pipeline` creates a function out of the input pipeline with each line
#' adding an additional step from the pipeline and calls the debugging browser
#' on the function. Each line run prints to the console plus stores the result
#' to a variable of the form `dot[N]`, where `[N]` is the index of the element
#' in the pipeline, so results can easily be replicated or adjusted in the
#' debugging console.
#'
#' `debug_pipeline` can also be called interactively in RStudio via the "Debug
#' pipeline in browser" add-in after highlighting the pipeline to debug in the
#' source editor.
#'
#' For a full explanation of the special commands available in the debugging
#' browser, see [`browser`].
#'
#' @param pipeline A pipeline to debug step-by-step Can be an expression or a
#'     string of code. If missing, uses the text highlighted in RStudio's
#'     source editor.
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' debug_pipeline(
#'     1:5 %>% rev() %>% {. * 2} %>% sample()
#' )
#' }
#'
#' @seealso [`browser`], [`magrittr::debug_pipe`]
#' @export
debug_pipeline <- function(pipeline){
    this_call <- match.call()
    if (missing(pipeline)) {
        pipeline <- rstudioapi::getSourceEditorContext()$selection[[1]]$text
    } else if (is.language(this_call[[2]])) {
        pipeline <- paste(trimws(deparse(this_call[[2]])), collapse = "")
    }
    if (nchar(pipeline) == 0) stop("Input unavailable. Did you highlight a pipeline?")

    pipe_parts <- strsplit(pipeline, "%>%[[:space:]]*")[[1]]
    pipe_parts <- Reduce(function(...) paste(..., sep = "%>%\n\t\t"),
                         pipe_parts,
                         accumulate = TRUE)
    pipe_lines <- paste("\tprint(dot", seq_along(pipe_parts), " <- ",
                        pipe_parts, ")", sep = "", collapse = "\n")

    pipeline_function <- eval(
        parse(text = paste("function(){", pipe_lines, "}", sep = "\n"))
    )
    debugonce(pipeline_function)
    pipeline_function()

    # so browser exits fully when called by add-in
    if (rstudioapi::isAvailable()) rstudioapi::sendToConsole("")
}
