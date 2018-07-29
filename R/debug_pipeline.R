#' Utility to grab pipeline expression from wherever available
#'
#' @param call An expression of the parent call, as captured by [`match.call`]
#' @param parse If `TRUE`, parses pipeline as [`debug_pipeline`]. If `FALSE`,
#'     takes pipeline as an already-parsed expression.
#' @inheritParams debug_pipeline
#' @keywords internal
get_pipeline <- function(pipeline, call, parse = TRUE, ...){
    if (!parse) {
        # don't re-parse
    } else if (missing(pipeline) && length(rlang::call_args(call)) == 0) {
        pipeline <- rstudioapi::getSourceEditorContext()$selection[[1]]$text
        if (nchar(pipeline) == 0) {
            stop("Input unavailable. Did you highlight a pipeline?", call. = FALSE)
        }
        pipeline <- rlang::parse_expr(pipeline)
        attr(pipeline, "rstudio") <- TRUE    # flag for replacement of source
    } else if (all(rlang::call_args_names(call) != "pipeline")) {
        # handle assignment with `=`
        call_names <- rlang::call_args_names(call)
        call_name <- call_names[!call_names %in% c("names", "parse")]
        pipeline <- rlang::call2("=", as.name(call_name), call[[call_name]])
    } else if (is.language(call$pipeline)) {
        if (!rlang::call_name(call$pipeline) %in% c("%>%", "<-", "=")) {
            call$pipeline <- eval(call$pipeline)
        }
        pipeline <- call$pipeline
    } else if (is.character(pipeline)) {
        pipeline <- paste(pipeline, collapse = "")
        if (nchar(pipeline) == 0) {
            stop("Input unavailable. Did you highlight a pipeline?", call. = FALSE)
        }
        pipeline <- rlang::parse_expr(pipeline)
    }
    pipeline
}

#' Split a pipeline expression into a list of expressions
#'
#' `split_pipeline` takes an expression of a pipeline and splits it into a list
#' of unevaluated calls.
#'
#' `split_pipeline` does not split nested pipelines.
#'
#' @inheritParams debug_pipeline
#'
#' @return A list of unevaluated calls extracted from the pipeline.
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' split_pipeline(
#'     quote(x <- 1:5 %>% rev %>% {. * 2} %>% sample(replace = TRUE))
#' )
#' }
#'
#' @seealso The unexported [\code{magrittr:::split_chain}](https://github.com/tidyverse/magrittr/blob/master/R/split_chain.R),
#'     does not handle assignment, but collects the particular pipes used.
#'     [`burst_pipes`] invisibly returns a restructured split pipeline.
#' @export
split_pipeline <- function(pipeline, ...){
    pipeline <- get_pipeline(pipeline, call = match.call(), ...)
    if (inherits(pipeline, "call") && rlang::call_name(pipeline) == "%>%") {
        c(split_pipeline(pipeline[[2]], parse = FALSE), pipeline[[3]])
    } else if (inherits(pipeline, c("<-", "="))) {
        x <- split_pipeline(pipeline[[3]], parse = FALSE)
        c(rlang::call_modify(pipeline[1:2], x[[1]]), x[-1])
    } else {
        pipeline
    }
}

#' Recursive helper function that replaces `.` within a call with a replacement
#' expression. Ignores formulas.
#'
#' @return A call with the dots replaced
#'
#' @param call A quoted call in which to replace dots
#' @param replacement A quoted name with which to replace dots
#'
#' @keywords internal
replace_dots <- function(call, replacement){
    if (!rlang::is_formula(call) && length(call) > 1) {
        call <- lapply(call, replace_dots, replacement = replacement)    # recurse
        call <- as.call(call)
    } else if (rlang::is_pairlist(call)) {    # for function formals
        call <- lapply(call, replace_dots, replacement = replacement)
        names(call)[names(call) == "."] <- as.character(replacement)
        call <- as.pairlist(call)
    } else if (call == as.name(".")) {
        call <- replacement    # replace
    }
    call
}

#' Pipeline parser helper function that returns a list of names and calls
#'
#' @return A list with `names` and `calls` elements. The `names` element
#'     contains `data` and `assign` sub-elements.
#'
#' @param ... Passed to [`split_pipeline`]
#' @inheritParams burst_pipes
#'
#' @seealso [`split_pipeline`], [`burst_pipes`]
#' @keywords internal
parse_pipeline <- function(pipeline, names, ...){
    pipe_lines <- split_pipeline(pipeline, ...)
    if (missing(names)) {
        names <- paste0("dot", seq_along(pipe_lines[-1]))
    }
    assign_names <- lapply(names, as.name)
    if (inherits(pipe_lines[[1]], c("<-", "="))) {
        # with assignment
        data_names <- c(pipe_lines[[1]][[3]], assign_names[-length(assign_names)])
        assign_names[[length(assign_names)]] <- pipe_lines[[1]][[2]]
    } else {
        # no assignment in pipeline
        data_names <- c(pipe_lines[[1]], assign_names[-length(assign_names)])
    }
    pipe_lines[[1]] <- NULL    # drop input data
    if (length(assign_names) != length(pipe_lines)) {
        stop("Number of names is not equal to number of top-level calls.",
             call. = FALSE)
    }

    # return parsed pipeline object
    list(
        names = list(
            data = data_names,
            assign = assign_names
        ),
        calls = pipe_lines
    )
}

#' Replace pipes with intermediate assignment
#'
#' `burst_pipes` rearranges a magrittr pipeline into equivalent unpiped code.
#' Called directly, it will print the restructured code to the console. Called
#' via the "Burst pipes" RStudio add-in while a pipeline is highlighted, it
#' will replace the highlighted code with the restructured code. The "Burst
#' pipes and set names" add-in opens a Shiny gadget in which names can be set.
#'
#' Note that nested pipelines are currently ignored. Calling on pipelines from
#' the inside out should still allow restructuring.
#'
#' Warning: Calling `burst_pipes` from the RStudio add-in is currently
#' irreversible aside from "Edit > Undo" and version control. Save code
#' beforehand and check equivalence afterwards.
#'
#' @return A list of expressions of the restructured, unpiped code, invisibly.
#'
#' @param names A character vector of names to be used for intermediary
#'     assignment of a length equal to that of the number of top-level calls in
#'     the pipeline.
#' @inheritParams debug_pipeline
#'
#' @seealso [`split_pipeline`] splits a pipeline without restructuring.
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' burst_pipes(x <- 1:5 %>% rev %>% {. * 2} %>% .[3] %>% rnorm(1, ., sd = ./10))
#'
#' burst_pipes(
#'     x = 1:5 %>% rev %>% {. * 2} %>% .[3] %>% rnorm(1, ., sd = ./10),
#'     names = c("reversed", "doubled", "third", "x")
#' )
#' }
#'
#' @export
burst_pipes <- function(pipeline, names, ...){
    pipeline <- get_pipeline(pipeline, call = match.call(), ...)
    parts <- parse_pipeline(pipeline, names, parse = FALSE)

    # Insert first-level data
    parts$calls <- Map(
        function(part, name){
            if (!inherits(part, "{") &&
                !any(vapply(part, `==`, logical(1), as.name(".")))) {
                if (is.symbol(part)) {
                    part <- rlang::call2(part)    # handle parentheses-less calls
                }
                rlang::expr(rlang::UQ(part[[1]])(    # function name
                    !!name,    # data piped in
                    !!!rlang::call_args(part))    # function parameters
                )
            } else {
                part    # data will be inserted as dot
            }
        },
        parts$calls,
        parts$names$data
    )

    # Insert data at dots
    parts$calls <- Map(
        replace_dots,
        call = parts$calls,
        replacement = parts$names$data
    )

    # Insert assignment
    parts$calls <- Map(
        function(part, name){
            substitute(n <- p, list(p = part, n = name))
        },
        parts$calls,
        parts$names$assign)

    # Visible returns
    parts$text <- paste(lapply(parts$calls, function(part){
        paste(trimws(deparse(part)), collapse = "")
    }), collapse = "\n")
    if (isTRUE(attr(pipeline, 'rstudio'))) {
        rstudioapi::modifyRange(
            rstudioapi::getSourceEditorContext()$selection[[1]]$range,
            parts$text
        )
    } else {
        cat(parts$text)
    }

    invisible(parts$calls)
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
#' @param pipeline A pipeline. Can be an unquoted or quoted expression, or a
#'     character vector of code. If missing, uses the text highlighted in
#'     RStudio's source editor.
#' @param ... Only used to allow assignment with `=` at the beginning of a
#'     pipeline.
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' debug_pipeline(
#'     x <- 1:5 %>% rev %>% {. * 2} %>% sample(replace = TRUE)
#' )
#' }
#'
#' @seealso [`browser`], [`magrittr::debug_pipe`], [`burst_pipes`]
#' @export
debug_pipeline <- function(pipeline, ...){
    pipeline <- get_pipeline(pipeline, call = match.call())

    pipe_lines <- burst_pipes(pipeline, parse = FALSE, ...)
    pipe_lines <- lapply(pipe_lines, function(part){
        rlang::expr(print(!!part))
    })

    pipeline_function <- rlang::new_function(
        args = NULL,
        body = rlang::expr({!!!pipe_lines})
    )
    debugonce(pipeline_function)
    pipeline_function()

    # so browser exits fully when called by add-in
    if (rstudioapi::isAvailable()) rstudioapi::sendToConsole("")
}
