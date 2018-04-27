context("debug_pipeline")

# *only* for use within `with_mock`; overwrites input function
debug_text <- function(f){
    assign(deparse(match.call()[[2]]),
           eval(substitute(function(...){out},
                           list(out = capture_output(f, print = TRUE)))),
           pos = 1)
}

test_that("pipelines are split", {
    expect_equal(split_pipeline(quote(1:5 %>% rev())),
                 alist(1:5, rev()))
    expect_equal(split_pipeline(quote(x <- 1:5 %>% rev())),
                 alist(x <- 1:5, rev()))
})

test_that("pipelines get debugged", {
    with_mock(
        debugonce = debug_text,
        expect_match(
            {
                capture.output(debug_pipeline(1:5 %>% rev(), data = "insert"))
                pipeline_function()
            },
            "print(dot2 <- rev(dot1))",
            fixed = TRUE
        ),
        expect_match(
            {
                capture.output(debug_pipeline(1:5 %>% rev(), data = "pipe"))
                pipeline_function()
            },
            "print(dot2 <- dot1 %>% rev())",
            fixed = TRUE
        ),
        expect_match(
            {
                capture.output(debug_pipeline("1:5 %>% rev()", data = "pipe"))
                pipeline_function()
            },
            "print(dot2 <- dot1 %>% rev())",
            fixed = TRUE
        )
    )
})

test_that("empty strings raise error", {
    expect_error(debug_pipeline(""),
                 "Input unavailable. Did you highlight a pipeline?")
})

test_that("missing input fails", {
    expect_error(debug_pipeline(),
                 "RStudio not running")
})
