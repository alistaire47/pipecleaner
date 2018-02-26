context("debug_pipeline")

# *only* for use within `with_mock`; overwrites input function
debug_text <- function(f){
    assign(deparse(match.call()[[2]]),
           eval(substitute(function(...){out},
                           list(out = capture_output(f, print = TRUE)))),
           pos = 1)
}

test_that("pipelines get debugged", {
    with_mock(
        debugonce = debug_text,
        expect_match(
            {
                capture.output(debug_pipeline(1:5 %>% rev()))
                pipeline_function()
            },
            "print(dot2 <- 1:5 %>% rev())",
            fixed = TRUE
        )
    )
})

test_that("empty strings raise error", {
    expect_error(debug_pipeline(""),
                 "Input unavailable. Did you highlight a pipeline?")
})
