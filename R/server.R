server <- function(input, output, session) {
    dat <- system.file(
        "extdata", "cMD_curated_metadata_release.csv",
        package = "cmdMeta", mustWork = TRUE
    ) |>
        utils::read.csv() |>
        dplyr::filter(
            !is.na(.data$country),
            !is.na(.data$body_site),
            !is.na(.data$bmi)
        ) |>
        dplyr::mutate(
            country = gsub(" ", "_", .data$country),
            body_site = gsub(" ", "_", .data$body_site)
        )

    visible_vars <- reactive({
        input$vars
    })

    country_counts <- shiny::reactive({
        data <- filtered_data()
        all_countries <- unique(dat$country)
        counts <- table(factor(data$country, levels = all_countries))
        data.frame(
            country = names(counts),
            n = as.numeric(counts),
            per = sprintf("%.1f%%", 100 * as.numeric(counts) / sum(counts))
        ) |>
            dplyr::arrange(-.data$n)
    })

    selected_classes <- shiny::reactiveVal(unique(dat$country))

    shiny::observe({
        class_list <- unique(dat$country)
        lapply(class_list, function(cls) {
            if (!is.null(input[[paste0("class_", make.names(cls))]])) {
                shiny::observeEvent(input[[paste0("class_", make.names(cls))]], {
                    current <- selected_classes()
                    if (input[[paste0("class_", make.names(cls))]]) {
                        selected_classes(unique(c(current, cls)))
                    } else {
                        selected_classes(setdiff(current, cls))
                    }
                })
            }
        })
    })

    filtered_data <- shiny::reactive({


        if ("body_site" %in% input$vars) {
            dat <- dat |>
                dplyr::filter(
                    .data$body_site %in% input$selected_vars
                )
        }

        if ("bmi" %in% input$vars) {
           dat <- dat |>
               dplyr::filter(
                    .data$bmi >= input$mpg_range[1],
                    .data$bmi <= input$mpg_range[2]
               )
        }

        if ("country" %in% input$vars) {
            selected <- selected_classes()
            if (length(selected) > 0) {
                dat <- dat |>
                    dplyr::filter(
                        .data$country %in% selected
                    )
            }
        }

        if (!length(input$vars) || !nrow(dat)) {
            return(dat[0,, drop = FALSE])
        }
        return(dat)
    })


    output$plots_tab <- shiny::renderUI({
        var_list <- visible_vars()

        if (length(var_list) == 0) return(NULL)

        shiny::fluidRow(
            purrr::map(var_list, function(id) {
                if (id == "body_site") {
                    shiny::column(
                        width = 3,
                        shinydashboard::box(
                            width = NULL,
                            title = "Body site",
                            solidHeader = TRUE,
                            class = "fixed-height-box",
                            htmltools::div(class = "plot-container",
                                           shiny::plotOutput("body_site_plot", height = "100%")
                            ),
                            htmltools::div(class = "controls-container",
                                           shinyWidgets::pickerInput(
                                               inputId = "selected_vars",
                                               label = NULL,
                                               choices = sort(unique(dat$body_site)),
                                               multiple = TRUE,
                                               selected = sort(unique(dat$body_site)),
                                               options = list(
                                                   `actions-box` = TRUE,
                                                   `live-search` = FALSE,
                                                   `selected-text-format` = "count > 1"
                                               )
                                           )
                                           # htmltools::div(
                                           #     style = "text-align: right;",
                                           #     shiny::actionButton("remove_A", "Remove", class = "btn-danger btn-sm")
                                           # )
                            )
                        )
                    )
                } else if (id == "bmi") {
                    shiny::column(
                        width = 3,
                        shinydashboard::box(
                            width = NULL,
                            title = "BMI Distribution",
                            status = NULL,
                            solidHeader = TRUE,
                            class = "fixed-height-box",
                            htmltools::div(
                                class = "plot-container",
                                shiny::plotOutput("mpg_plot", height = "100%")
                            ),
                            htmltools::div(
                                class = "controls-container",
                                shiny::sliderInput(
                                    inputId = "mpg_range",
                                    label = NULL,
                                    min = floor(min(dat$bmi)),
                                    max = ceiling(max(dat$bmi)),
                                    value = c(floor(min(dat$bmi)), ceiling(max(dat$bmi))),
                                    step = 1
                                )
                                # htmltools::div(
                                #     style = "text-align: right;"
                                #     # shiny::actionButton("remove_B", "Remove", class = "btn-danger btn-sm")
                                # )
                            )
                        )
                    )
                } else if (id == "country") {
                    shiny::column(
                        width = 3,
                        shinydashboard::box(
                            width = NULL,
                            title = "Country",
                            solidHeader = TRUE,
                            class = "fixed-height-box",
                            htmltools::div(
                                class = "table-container",
                                shiny::uiOutput("class_summary")
                            )
                            # div(class = "controls-container",
                            #     div(
                            #         style = "text-align: right;",
                            #         actionButton("remove_C", "Remove", class = "btn-danger btn-sm")
                            #     )
                            # )
                        )
                    )
                }
            })
        )
    })

    output$table_tab <- DT::renderDT({
        data <- filtered_data()
        dt <- DT::datatable(
            data = data,
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip'
            ),
            rownames = FALSE,
            caption = "curatedMetagenomicData samples"
        )
        return(dt)
    })

    output$class_summary <- renderUI({
        counts <- country_counts()
        htmltools::tags$table(
            class = "summary-table",
            htmltools::tags$thead(
                htmltools::tags$tr(
                    htmltools::tags$th("Select"),
                    htmltools::tags$th("country"),
                    htmltools::tags$th("n"),
                    htmltools::tags$th("%")
                )
            ),
            htmltools::tags$body(
                lapply(1:nrow(counts), function(i) {
                    cls <- counts$country[i]
                    htmltools::tags$tr(
                        htmltools::tags$td(
                            class = "checkbox-cell",
                            shiny::checkboxInput(
                                inputId = paste0("class_", make.names(cls)),
                                label = NULL,
                                value = cls %in% selected_classes()
                            )
                        ),
                        tags$td(cls),
                        tags$td(counts$n[i]),
                        tags$td(counts$per[i]),
                    )
                })
            )
        )
    })

    output$body_site_plot <- shiny::renderPlot({
        plotFun(filtered_data(), "body_site")
    }, res = 80)

    output$mpg_plot <- shiny::renderPlot({
        plotFun(filtered_data(), "bmi")
    }, res = 80)
}
