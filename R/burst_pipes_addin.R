# Shiny `burst_pipes` RStudio add-in
burst_pipes_addin <- function() {
    pipeline_text <- rstudioapi::getSourceEditorContext()$selection[[1]]$text
    if (pipeline_text == "") {
        pipeline_init <- ""
        initial_tab <- "Change pipeline"
    } else {
        pipeline_expr <- rlang::parse_expr(pipeline_text)
        n_lines <- length(split_pipeline(pipeline_expr, parse = FALSE)) - 1
        pipeline_init <- utils::capture.output(
            burst_pipes(pipeline_init, parse = FALSE)
        )
        initial_tab <- "Set names"
    }

    ui <- miniUI::miniPage(
        shiny::div(style = "display:none;", shinyAce::aceEditor("invisible")),
        miniUI::gadgetTitleBar(
            "Burst pipes",
            right = miniUI::miniTitleBarButton("done", "Replace code", primary = TRUE)
        ),
        miniUI::miniTabstripPanel(
            id = "tab",
            selected = initial_tab,
            miniUI::miniTabPanel(
                "Change pipeline", value = "Change pipeline",
                icon = shiny::icon("bars"),
                miniUI::miniContentPanel(shinyAce::aceEditor(
                    "pipeline_code",
                    value = pipeline_text,
                    mode = "r",
                    theme = "tomorrow",
                    wordWrap = TRUE,
                    autoComplete = "enabled"
                ))
            ),
            miniUI::miniTabPanel(
                "Set names", value = "Set names",
                icon = shiny::icon("list-ul"),
                miniUI::miniContentPanel(
                    shiny::fillRow(
                        flex = c(1, 2),
                        shiny::code(
                            style = "padding: 0px;",
                            shiny::uiOutput("text_boxes")
                        ),
                        shinyAce::aceEditor(
                            "code",
                            value = pipeline_init,
                            mode = "r",
                            theme = "tomorrow",
                            wordWrap = TRUE,
                            autoComplete = "enabled"
                        )
                    )
                )
            )
        )
    )

    server <- function(input, output, session) {

        pipeline <- shiny::reactive({
            tryCatch(
                rlang::parse_expr(paste(input$pipeline_code, collapse = " ")),
                error = function(e) structure(
                    quote("Invalid input" %>% print()),
                    invalid = TRUE
                )
            )
        })

        n_lines <- shiny::reactive({
            length(split_pipeline(pipeline(), parse = FALSE)) - 1
        })

        shiny::observe({
            output$text_boxes <- shiny::renderUI(lapply(seq(n_lines()), function(i){
                shiny::textInput(
                    inputId = paste0("name", i),
                    label = if (i == 1) "Names:" else NULL,
                    value = paste0("dot", i),
                    width = "90%"
                )
            }))
        })

        line_names <- shiny::reactive({
            names <- vapply(
                grep("^name\\d+$", names(input), value = TRUE),
                function(n) input[[n]],
                character(1)
            )
            names[names == ""] <- paste0("dot", seq(n_lines))[names == ""]
            names
        })

        shiny::observe({
            if (input$tab == "Set names") {
                # ensure #names == #calls
                dot_names <- paste0('dot', seq(n_lines()))
                i <- seq(min(n_lines(), length(line_names())))
                dot_names[i] <- line_names()[i]

                if (isTRUE(attr(pipeline(), "invalid"))) {
                    value <- "Error: Invalid input"
                } else {
                    value <- paste(utils::capture.output(
                        burst_pipes(pipeline(), dot_names, parse = FALSE)
                    ), collapse = "\n")
                }

                shinyAce::updateAceEditor(session, "code", value = value)
            }
        })

        # replace text and close
        shiny::observeEvent(input$done, {

            rstudioapi::modifyRange(
                rstudioapi::getSourceEditorContext()$selection[[1]]$range,
                input$code
            )
            shiny::stopApp()
        })
    }

    shiny::runGadget(ui, server)

}
