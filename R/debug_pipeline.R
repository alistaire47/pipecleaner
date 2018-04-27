#' Split a pipeline expression into a list of expressions
#'
#' `split_pipeline` takes an unevaluated expression of a pipeline and splits it
#' into a list of expressions.
#'
#' Here "expression" is use colloquially to refer to a bare language object,
#' i.e. like those returned by [`quote`] or [`rlang::expr`], _not_ an object of
#' class "expression" like those returned by [`expression`]. The latter can be
#' turned into the former by subsetting, e.g. `expression(1:5 %>% rev())[[1]]`.
#'
#' `split_pipeline` does not split nested pipelines.
#'
#' @param pipeline An unevaluated expression of a pipeline to be split.
#'
#' @return A list of unevaluated expressions of each element piped together.
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' split_pipeline(
#'     quote(x <- 1:5 %>% rev %>% {. * 2} %>% sample(replace = TRUE))
#' )
#' }
#'
#' @seealso The unexported [\code{magrittr:::split_chain}](https://github.com/tidyverse/magrittr/blob/master/R/split_chain.R),
#'     which does not handle assignment, but collects the particular pipes used.
#' @export
split_pipeline <- function(pipeline){
    if (inherits(pipeline, "call") && rlang::call_name(pipeline) == "%>%") {
        c(split_pipeline(pipeline[[2]]), pipeline[[3]])
    } else if (inherits(pipeline, "<-")) {
        x <- split_pipeline(pipeline[[3]])
        c(rlang::call_modify(pipeline[1:2], x[[1]]), x[-1])
    } else {
        pipeline
    }
}


#' Run a pipeline step-by-step in the debugging browser
#'
#' `debug_pipeline` creates a function out of the input pipeline with each line
#' running an additional step from the pipeline and calls the debugging browser
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
#' @param pipeline A pipeline to debug step-by-step Can be an unquoted
#'     expression or a character vector of code. If missing, uses the text
#'     highlighted in RStudio's source editor.
#' @param data Determines whether data is piped in (`data %>%
#'     function(parameter)`) or inserted as the first parameter of each call
#'     (`function(data, parameter)`). Piping is necessary to make dot notation
#'     work; inserting makes stepping into calls easier.
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' debug_pipeline(
#'     x <- 1:5 %>% rev %>% {. * 2} %>% sample(replace = TRUE)
#' )
#'
#' debug_pipeline(
#'     x <- 1:5 %>% rev() %>% sample(replace = TRUE),
#'     data = "insert"
#' )
#' }
#'
#' @seealso [`browser`], [`magrittr::debug_pipe`]
#' @export
debug_pipeline <- function(pipeline, data = c("pipe", "insert")){
    this_call <- match.call()
    data = match.arg(data)

    if (missing(pipeline)) {
        pipeline <- rstudioapi::getSourceEditorContext()$selection[[1]]$text
        pipeline <- rlang::parse_expr(pipeline)
    } else if (is.character(pipeline) && nchar(pipeline) > 0) {
        pipeline <- paste(pipeline, collapse = "")
        pipeline <- rlang::parse_expr(pipeline)
    } else if (is.language(this_call[[2]])) {
        pipeline <- this_call[[2]]
    } else {
        stop("Input unavailable. Did you highlight a pipeline?")
    }

    pipe_parts <- split_pipeline(pipeline)
    pipe_parts[[1]] <- rlang::expr(print(dot1 <- !!pipe_parts[[1]]))
    pipe_parts[-1] <- Map(
        function(line, i){
            if (data == "insert") {
                name0 <- as.name(paste0("dot", i))
                name1 <- as.name(paste0("dot", i + 1))
                line <- rlang::expr(rlang::UQ(line[[1]])(    # function name
                    !!name0,    # data piped in
                    !!!rlang::call_args(line))    # function parameters
                )
                substitute(print(nm1 <- ln),
                           list(nm1 = name1,
                                ln = line))
            } else {
                substitute(print(nm1 <- nm0 %>% ln),
                           list(nm0 = as.name(paste0("dot", i)),
                                nm1 = as.name(paste0("dot", i + 1)),
                                ln = line))
            }
        },
        line = pipe_parts[-1],
        i = seq_along(pipe_parts[-1])
    )

    pipeline_function <- rlang::new_function(
        args = NULL,
        body = rlang::expr({!!!pipe_parts})
    )
    debugonce(pipeline_function)
    pipeline_function()

    # so browser exits fully when called by add-in
    if (rstudioapi::isAvailable()) rstudioapi::sendToConsole("")
}

debug_pipeline_insert = debug_pipeline
rlang::fn_fmls(debug_pipeline_insert)$data <- "insert"

debug_pipeline_pipe = debug_pipeline
rlang::fn_fmls(debug_pipeline_pipe)$data <- "pipe"
