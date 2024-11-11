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


    # reactive_list_2 <- shiny::reactiveValues()
    #
    # shiny::observe({
    #     visVars <- visible_vars()
    #     names(visVars) <- visVars
    #     reactive_list_2$list <- purrr::map(visVars, ~ {
    #         classVar <- metadataVars[[.x]]$class
    #         lenVar <- metadataVars[[.x]]$length
    #         if (classVar == "character" && lenVar > 7) {
    #             r <- shiny::reactiveVal(unique(dat[[.x]]))
    #             message(paste0(.x, ">>>>>>"))
    #             return(r)
    #         }
    #         return(NULL)
    #     }) |>
    #         purrr::discard(is.null)
    # })

    selected_country <- shiny::reactiveVal(unique(dat$country))
    shiny::observe({
        class_list <- unique(dat$country)
        lapply(class_list, function(cls) {
            if (!is.null(input[[paste0("class_country_", make.names(cls))]])) {
                shiny::observeEvent(input[[paste0("class_country_", make.names(cls))]], {
                    current <- selected_country()
                    # current <- reactive_list_2$list[["country"]]()
                    if (input[[paste0("class_country_", make.names(cls))]]) {
                        selected_country(unique(c(current, cls)))
                        # reactive_list_2$list[["country"]](unique(c(current, cls)))
                    } else {
                        selected_country(setdiff(current, cls))
                        # reactive_list_2$list[["country"]](setdiff(current, cls))
                    }
                })
            }
        })
    })





    ## Filter data ####
    filtered_data <- shiny::reactive({
        data <- dat
        for (i in input$vars) {
            classVar <- metadataVars[[i]]$class
            lenVar <- metadataVars[[i]]$length

            if (classVar == "character" && lenVar <= 7) {
                data <- data |>
                    dplyr::filter(
                        .data[[i]] %in% input[[paste0(i, "_menu")]]
                    )

            } else if (classVar == "character" && lenVar > 7) {
                data <- data

            } else if (classVar == "numeric" || classVar == "integer") {
                data <- data |>
                    dplyr::filter(
                        .data[[i]] >= input[[paste0(i, "_range")]][1],
                        .data[[i]] <= input[[paste0(i, "_range")]][2]
                    )

            }

        }

        if ("country" %in% input$vars) {
            selected <- selected_country()
            # selected <- reactive_list_2$list[["country"]]()
            if (length(selected) > 0) {
                data <- data |>
                    dplyr::filter(
                        .data$country %in% selected
                    )
            }
        }

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
                    summaryFun(.x, counts, selected_country())
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
