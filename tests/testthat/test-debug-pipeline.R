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
    expect_equal(split_pipeline(1:5 %>% rev()),
                 alist(1:5, rev()))
    expect_equal(split_pipeline(x <- 1:5 %>% rev()),
                 alist(x <- 1:5, rev()))
    expect_equal(split_pipeline(x = 1:5 %>% rev()),
                 list(quote({x = 1:5})[[-1]], quote(rev())))
    expect_equal(split_pipeline("x <- 1:5 %>% rev()"),
                 alist(x <- 1:5, rev()))
})

test_that("pipes get burst", {
    with_mock(
        cat = function(...){},    # don't clutter test reporter output
        expect_equal(burst_pipes(1:5 %>% rev %>% {. * 2} %>% .[3]),
                     alist(dot1 <- rev(1:5), dot2 <- {dot1 * 2}, dot3 <- dot2[3])),
        expect_equal(burst_pipes(x <- 1:5 %>% rev %>% {. * 2} %>% .[3]),
                     alist(dot1 <- rev(1:5), dot2 <- {dot1 * 2}, x <- dot2[3])),
        expect_equal(burst_pipes(x = 1:5 %>% rev %>% {. * 2} %>% .[3]),
                     alist(dot1 <- rev(1:5), dot2 <- {dot1 * 2}, x <- dot2[3])),
        expect_equal(burst_pipes("x = 1:5 %>% rev %>% {. * 2} %>% .[3]"),
                     alist(dot1 <- rev(1:5), dot2 <- {dot1 * 2}, x <- dot2[3])),
        expect_equal(burst_pipes(x <- 1:5 %>% rev %>% {. * 2} %>% .[3],
                                 names = c("one", "two", "three")),
                     alist(one <- rev(1:5), two <- {one * 2}, x <- two[3])),
        expect_error(burst_pipes(x <- 1:5 %>% rev %>% {. * 2} %>% .[3],
                                 names = c("one", "two")),
                     "Number of names is not equal to number of top-level calls"),
        # ensure no evaluation happens
        expect_lt(system.time(burst_pipes(1 %>% Sys.sleep()))["elapsed"], 1)
    )
})

test_that("dot substitution works", {
    with_mock(
        cat = function(...){},
        expect_equal(burst_pipes(1:5 %>% rev), alist(dot1 <- rev(1:5))),
        expect_equal(burst_pipes(1:5 %>% rev()), alist(dot1 <- rev(1:5))),
        expect_equal(burst_pipes((1:5) %>% .[3]), alist(dot1 <- (1:5)[3])),
        expect_equal(burst_pipes(mtcars %>% lm(mpg ~ ., .)),
                     alist(dot1 <- lm(mpg ~ ., mtcars))),
        expect_equal(burst_pipes(1:5 %>% {. * 2 + .}),
                     alist(dot1 <- {1:5 * 2 + 1:5})),
        expect_equal(
            burst_pipes(mtcars %>% lapply(function(x){
                mean(x) == max(colMeans(.))
        })),
            alist(dot1 <- lapply(mtcars, function(x) {
                mean(x) == max(colMeans(mtcars))
            })))
    )
})

test_that("pipelines get debugged", {
    with_mock(
        debugonce = debug_text,
        expect_match(
            {
                capture.output(debug_pipeline(1:5 %>% rev()))
                pipeline_function()
            },
            "print(dot1 <- rev(1:5))",
            fixed = TRUE
        ),
        expect_match(
            {
                capture.output(debug_pipeline("1:5 %>% rev()"))
                pipeline_function()
            },
            "print(dot1 <- rev(1:5))",
            fixed = TRUE
        )
    )
})

test_that("missing input fails", {
    expect_error(split_pipeline(""),
                 "Input unavailable. Did you highlight a pipeline?")
    expect_error(burst_pipes(""),
                 "Input unavailable. Did you highlight a pipeline?")
    expect_error(debug_pipeline(""),
                 "Input unavailable. Did you highlight a pipeline?")
    expect_error(split_pipeline(),
                 "RStudio not running|Input unavailable")
    expect_error(burst_pipes(),
                 "RStudio not running|Input unavailable")
    expect_error(debug_pipeline(),
                 "RStudio not running|Input unavailable")
})
