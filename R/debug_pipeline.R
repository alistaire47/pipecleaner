split_pipes <- function(pipeline){
    if (inherits(pipeline, c("call", "<-"))) {
        if (rlang::call_name(pipeline) == "%>%") {
            c(split_pipes(pipeline[[2]]), pipeline[[3]])
        } else {
            x <- split_pipes(pipeline[[3]])
            c(rlang::call_modify(pipeline[1:2], x[[1]]), x[-1])
        }
    } else {pipeline}
}


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
        pipeline <- rlang::parse_expr(pipeline)
    } else if (is.language(this_call[[2]])) {
        pipeline <- this_call[[2]]
    } else {
        stop("Input unavailable. Did you highlight a pipeline?")
    }

    pipe_parts <- split_pipes(pipeline)
    pipe_lines <- Reduce(function(...) rlang::expr(!!..1 %>% !!..2),
                         pipe_parts,
                         accumulate = TRUE)
    pipe_lines <- Map(
        function(line, i){
            # use base because R CMD check complains about expr with `<-`
            substitute(print(nm <- ln),
                       list(nm = as.name(paste0('dot', i)),
                            ln = line))
        },
        pipe_lines,
        seq_along(pipe_lines)
    )

    pipeline_function <- rlang::new_function(
        args = NULL,
        body = rlang::expr({!!!pipe_lines})
    )
    debugonce(pipeline_function)
    pipeline_function()

    # so browser exits fully when called by add-in
    if (rstudioapi::isAvailable()) rstudioapi::sendToConsole("")
}
