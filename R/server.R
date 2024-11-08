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














    # class_counts <- shiny::reactive({
    #     data <- filtered_data()
    #     all_classes <- unique(dat$country)
    #     counts <- table(factor(data$country, levels = all_classes))
    #     data.frame(
    #         Class = names(counts),
    #         Count = as.numeric(counts),
    #         Percentage = sprintf("%.1f%%", 100 * as.numeric(counts) / sum(counts))
    #     ) %>%
    #         arrange(desc(Count))
    # })

    # selected_classes <- reactiveVal(unique(dat$country))

    # observe({
    #     class_list <- unique(dat$country)
    #     lapply(class_list, function(cls) {
    #         if (!is.null(input[[paste0("class_", make.names(cls))]])) {
    #             observeEvent(input[[paste0("class_", make.names(cls))]], {
    #                 current <- selected_classes()
    #                 if (input[[paste0("class_", make.names(cls))]]) {
    #                     selected_classes(unique(c(current, cls)))
    #                 } else {
    #                     selected_classes(setdiff(current, cls))
    #                 }
    #             })
    #         }
    #     })
    # })

    filtered_data <- shiny::reactive({

        if ("body_site" %in% input$vars)
            dat <- dat |>
                dplyr::filter(
                    .data$body_site %in% input$selected_vars
                )

        if ("bmi" %in% input$vars)
           dat <- dat |>
               dplyr::filter(
                    .data$bmi >= input$mpg_range[1],
                    .data$bmi <= input$mpg_range[2]
               )

        # selected <- selected_classes()
        # if (length(selected) > 0) {
        #     data <- data[data$country %in% selected, ]
        # }

        # if (!is.null(input$selected_vars) && length(input$selected_vars) > 0) {
            # message(paste(input$selected_vars, collapse = ", "))
            # message(length(input$selected_vars))
            # data <- data[data$body_site %in% input$selected_vars,]
        # }


        # if (nrow(data) == 0) {
        #     return(data[0,])
        # }

        # message(nrow(data))
        return(dat)
    })

    generate_cyl_plot <- function() {
        data <- filtered_data()
        if(nrow(data) == 0) {
            p <- ggplot2::ggplot() +
                ggplot2::theme_void() +
                ggplot2::annotate("text", x = 0, y = 0, label = "No data available")
            return(p)
        }

        data_summary <- data |>
            dplyr::count(.data$body_site) |>
            dplyr::mutate(
                n = round(n / sum(n) * 100)
            )

        p <- data_summary |>
            ggplot2::ggplot(
                mapping = ggplot2::aes(
                    x = stats::reorder(body_site, -n), y = n
                )
            ) +
            ggplot2::geom_col() +
            ggplot2::scale_y_continuous(limits = c(0, 100)) +
            ggplot2::labs(
                x = "Body site",
                y = "% Samples"
            )
        return(p)
    }

    generate_mpg_plot <- function() {
        data <- filtered_data()
        if(nrow(data) == 0) {
            emptyPlot <- ggplot2::ggplot() +
                ggplot2::theme_void() +
                ggplot2::annotate("text", x = 0, y = 0, label = "No data available")
            return(emptyPlot)
        }

        ggplot2::ggplot(data, ggplot2::aes(x = bmi)) +
            ggplot2::geom_histogram() +
            ggplot2::labs(
                x = "Body mass index (BMI)",
                y = "Count"
            )
    }

    shiny::observe({
        var_list <- visible_vars()
        lapply(var_list, function(id) {
            button_id <- paste0("remove_", id)
            shiny::observeEvent(input[[button_id]], {
                current_selection <- input$vars
                new_selection <- setdiff(current_selection, id)
                shinyWidgets::updatePickerInput(session, "vars", selected = new_selection)
            }, ignoreInit = TRUE)
        })
    })

    output$table_tab <- DT::renderDT({
        data <- filtered_data()
        message(nrow(data))
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

    # Create the class summary table output with checkboxes
    # output$class_summary <- renderUI({
        # counts_df <- class_counts()

    #     tags$table(
    #         class = "summary-table",
    #         tags$thead(
    #             tags$tr(
    #                 tags$th("Select"),
    #                 tags$th("Vehicle Class"),
    #                 tags$th("Count"),
    #                 tags$th("Percentage")
    #             )
    #         ),
    #         tags$tbody(
    #             lapply(1:nrow(counts_df), function(i) {
    #                 cls <- counts_df$country[i]
    #                 tags$tr(
    #                     tags$td(
    #                         class = "checkbox-cell",
    #                         checkboxInput(
    #                             inputId = paste0("class_", make.names(cls)),
    #                             label = NULL,
    #                             value = cls %in% selected_classes()
    #                         )
    #                     ),
    #                     tags$td(cls),
    #                     tags$td(counts_df$Count[i]),
    #                     tags$td(counts_df$Percentage[i])
    #                 )
    #             })
    #         )
    #     )
    # })

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
                                    options = list(`actions-box` = TRUE)
                                ),
                                htmltools::div(
                                    style = "text-align: right;",
                                    shiny::actionButton("remove_A", "Remove", class = "btn-danger btn-sm")
                                )
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
                            collapsible = FALSE,
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
                                ),
                                htmltools::div(
                                    style = "text-align: right;",
                                    shiny::actionButton("remove_B", "Remove", class = "btn-danger btn-sm")
                                )
                            )
                        )
                    )
                # } else if (id == "country") {
                    # column(
                    #     width = 4,
                    #     box(
                    #         width = NULL,
                    #         title = "Vehicle Class Distribution",
                    #         status = "primary",
                    #         solidHeader = TRUE,
                    #         collapsible = TRUE,
                    #         class = "fixed-height-box",
                    #         div(class = "table-container",
                    #             uiOutput("class_summary")
                    #         ),
                    #         div(class = "controls-container",
                    #             div(
                    #                 style = "text-align: right;",
                    #                 actionButton("remove_C", "Remove", class = "btn-danger btn-sm")
                    #             )
                    #         )
                    #     )
                    # )
                }
            })
        )
    })

    output$body_site_plot <- shiny::renderPlot({
        generate_cyl_plot()
    }, res = 80)

    output$mpg_plot <- shiny::renderPlot({
        generate_mpg_plot()
    })
}
