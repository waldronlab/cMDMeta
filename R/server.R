server <- function(input, output, session) {

    ## Data ####
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

    visible_vars <- shiny::reactive({
        input$vars
    })

    reactive_list <- shiny::reactiveValues()
    shiny::observe({
        visVars <- visible_vars()
        names(visVars) <- visVars
        reactive_list$list <- purrr::map(visVars, ~ {
            classVar <- metadataVars[[.x]]$class
            lenVar <- metadataVars[[.x]]$length
            if (classVar == "character" && lenVar > 7) {
                data <- filtered_data()
                all_vals <- unique(dat[[.x]])
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

    ###########################################################################
    # selected_country <- shiny::reactiveVal(unique(dat$country))
    # shiny::observe({
    #     class_list <- unique(dat$country)
    #     lapply(class_list, function(cls) {
    #         if (!is.null(input[[paste0("class_country_", make.names(cls))]])) {
    #             shiny::observeEvent(input[[paste0("class_country_", make.names(cls))]], {
    #                 # current <- selected_country()
    #                 # current <- reactive_list_2$list[["country"]]()
    #                 current <- reactive_values_2[["country"]]
    #                 if (input[[paste0("class_country_", make.names(cls))]]) {
    #                     # selected_country(unique(c(current, cls)))
    #                     # reactive_list_2$list[["country"]](unique(c(current, cls)))
    #                     reactive_values_2[["country"]] <- unique(c(current, cls))
    #                 } else {
    #                     # selected_country(setdiff(current, cls))
    #                     # reactive_list_2$list[["country"]](setdiff(current, cls))
    #                     reactive_values_2[["country"]] <- setdiff(current, cls)
    #                 }
    #             })
    #         }
    #     })
    # })

    observe({
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
                            observeEvent(input[[input_id]], {
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











   ############################################################################

    reactive_values_2 <- reactiveValues()
    observe({
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

    ## Filter data ####
    filtered_data <- shiny::reactive({
        data <- dat
        shiny::req(input$vars)
        for (i in input$vars) {
            classVar <- metadataVars[[i]]$class
            lenVar <- metadataVars[[i]]$length

            if (classVar == "character" && lenVar <= 7) {
                data <- data |>
                    dplyr::filter(
                        .data[[i]] %in% input[[paste0(i, "_menu")]]
                    )

            } else if (classVar == "character" && lenVar > 7) {
                if (!is.null(reactive_values_2[[i]])) {
                    selected <- reactive_values_2[[i]]
                    print(selected)
                    if (length(selected) > 0) {
                        data <- data |>
                            dplyr::filter(
                                .data[[i]] %in% selected
                            )
                    }
                }

            } else if (classVar == "numeric" || classVar == "integer") {
                data <- data |>
                    dplyr::filter(
                        .data[[i]] >= input[[paste0(i, "_range")]][1],
                        .data[[i]] <= input[[paste0(i, "_range")]][2]
                    )

            }

        }

        # if ("country" %in% input$vars) {
        #     selected <- selected_country()
        #     if (length(selected) > 0) {
        #         data <- data |>
        #             dplyr::filter(
        #                 .data$country %in% selected
        #             )
        #     }
        # }

        if (!length(input$vars) || !nrow(data)) {
            return(data[0,, drop = FALSE])
        }
        return(data)
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
                    # summaryFun(.x, counts, selected_country())
                    summaryFun(.x, counts, reactive_values_2[[.x]])
                    # summaryFun(.x, counts, reactive_list_2$list[["country"]]())
                })
                return(NULL)
            }
        }) |>
            purrr::discard(is.null)
    })

    ## Plots ####
    output$plots_tab <- shiny::renderUI({
        var_list <- visible_vars()
        if (!length(var_list)) {
            return(NULL)
        }
        shiny::fluidRow(
            purrr::map(var_list, ~ boxFun(.x, dat)) |>
                purrr::discard(is.null)
        )
    })

    ## Table ####
    output$table_tab <- DT::renderDT({
        data <- filtered_data()
        DT::datatable(
            data = data,
            rownames = FALSE
        )
    })

}
