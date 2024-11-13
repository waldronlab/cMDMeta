server <- function(input, output, session) {
    dat <- metadata
    visible_vars <- shiny::reactive(input$vars)
    reactive_list <- shiny::reactiveValues()
    reactive_values_2 <- shiny::reactiveValues()


    filtered_data <- shiny::reactive({
        ## This code chunck will run until a box has been created
        shiny::req(any(grepl("(_menu|class_|_range)", names(input))))
        data <- dat
        for (i in input$vars) {
            classVar <- metadataVars[[i]]$class
            lenVar <- metadataVars[[i]]$length
            if (classVar == "character" && lenVar <= 7) {
                data <- data |>
                    dplyr::filter(
                        .data[[i]] %in% input[[paste0(i, "_menu")]]
                    )
                print(head(data))
                message(i)
                message(dim(data))

            } else if (classVar == "character" && lenVar > 7) {
                if (!is.null(reactive_values_2[[i]])) {
                    selected <- reactive_values_2[[i]]
                    if (length(selected) > 0) {
                        data <- data |>
                            dplyr::filter(
                                .data[[i]] %in% selected
                            )
                        # message(dim(data))
                    }
                }
            } else if (classVar == "numeric" || classVar == "integer") {
                shiny::req(input[[paste0(i, "_range")]])
                data <- data |>
                    dplyr::filter(
                        .data[[i]] >= input[[paste0(i, "_range")]][1],
                        .data[[i]] <= input[[paste0(i, "_range")]][2]
                    )
            }
        }
        if (!nrow(data)) {
            return(data[0,, drop = FALSE])
        }
        return(data)
    })



    ## Create summary for counttries
    shiny::observe({
        visVars <- visible_vars()
        names(visVars) <- visVars
        reactive_list$list <- purrr::map(visVars, ~ {
            classVar <- metadataVars[[.x]]$class
            lenVar <- metadataVars[[.x]]$length
            if (classVar == "character" && lenVar > 7) {
                data <- filtered_data()
                all_vals <- unique(dat[[.x]])
                all_vals <- all_vals[!is.na(all_vals)]
                all_vals <- all_vals[all_vals != "NA"]
                counts <- table(factor(data[[.x]], levels = all_vals))
                df <- data.frame(
                    x = names(counts),
                    n = as.numeric(counts),
                    per = sprintf("%.1f%%", 100 * as.numeric(counts) / sum(counts))
                ) |>
                    dplyr::arrange(-.data$n)
                names(df)[1] <- .x
                return(df)

            }
            return(NULL)
        }) |>
            purrr::discard(is.null)
    })



    ## Create filtres for countires
    shiny::observe({
        visVars <- visible_vars()

        # Loop through visible variables
        for (var in visVars) {
            local({
                local_var <- var  # Create local copy for proper scoping

                # Check if variable meets criteria
                if (metadataVars[[local_var]]$class == "character" &&
                    metadataVars[[local_var]]$length > 7) {

                    # Get unique values for this variable
                    class_list <- unique(dat[[local_var]])

                    # Create observers for each value
                    lapply(class_list, function(cls) {
                        input_id <- paste0("class_", local_var, "_", make.names(cls))

                        if (!is.null(input[[input_id]])) {
                            shiny::observeEvent(input[[input_id]], {
                                current <- reactive_values_2[[local_var]]
                                if (input[[input_id]]) {
                                    reactive_values_2[[local_var]] <- unique(c(current, cls))
                                } else {
                                    reactive_values_2[[local_var]] <- setdiff(current, cls)
                                }
                            })
                        }
                    })
                }
            })
        }
    })

    shiny::observe({
        visVars <- visible_vars()
        for (var in visVars) {
            classVar <- metadataVars[[var]]$class
            lenVar <- metadataVars[[var]]$length
            if (classVar == "character" && lenVar > 7) {
                reactive_values_2[[var]] <- unique(dat[[var]])
            }
        }
        for (var in names(reactive_values_2)) {
            if (!(var %in% visVars)) {
                reactive_values_2[[var]] <- NULL
            }
        }
    })

    output$box_studies <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = length(unique(filtered_data()$study_name)),
            subtitle = "Studies",
            icon = shiny::icon("flask"),
            color = "purple"
        )
    })

    output$box_samples <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = format(length(unique(filtered_data()$ncbi_accession)), big.mark = ","),
            subtitle = "NCBI accession numbers",
            icon = shiny::icon("database"),
            color = "olive"
        )
    })

    ## Outputs in boxes ####
    shiny::observe({
        purrr::map(visible_vars(), ~ {
            classVar <- metadataVars[[.x]]$class
            lenVar <- metadataVars[[.x]]$length
            if (classVar == "numeric" || classVar == "integer") {
                outputName <- make.names(paste0(.x, "_plot"))
                output[[outputName]] <- shiny::renderPlot({
                    plotFun(filtered_data(), .x)
                }, res = 80)
            } else if (classVar == "character" && lenVar <= 7) {
                outputName <- make.names(paste0(.x, "_plot"))
                output[[outputName]] <- shiny::renderPlot({
                    plotFun(filtered_data(), .x)
                }, res = 80)
            } else if (classVar == "character" && lenVar > 7) {
                outputName <- make.names(paste0(.x, "_summary"))
                output[[outputName]] <- shiny::renderUI({
                    counts <- reactive_list$list[[.x]]
                    summaryFun(.x, counts, reactive_values_2[[.x]])
                })
                return(NULL)
            }
        }) |>
            purrr::discard(is.null)
    })

    output$plots_tab <- shiny::renderUI({
        shiny::req(input$vars)
        shiny::fluidRow(
            purrr::map(input$vars, ~ boxFun(.x, dat)) |>
                purrr::discard(is.null)
        )
    })

    output$table_tab <- DT::renderDT({
        shiny::req(input$vars)
        DT::datatable(
            data = filtered_data(),
            rownames = FALSE,
            options = list(scrollX = TRUE)
        )
    })
}
