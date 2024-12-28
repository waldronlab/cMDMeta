
server <- function(input, output, session) {
    ## metadat and metadataVars are saved in R/sysdata.rda
    dat <- metadata
    
    reactive_list <- shiny::reactiveValues()
    reactive_values_2 <- shiny::reactiveValues()
    
    filtered_data <- shiny::reactive({
        shiny::req(input$vars)
        shiny::req(dat)
        shiny::req(any(grepl("(_menu|class_|_range)", names(input))))
        
        data <- dat
        
        for (ivar in input$vars) {
            varLab <- metadataVars[[ivar]]$label
            if (varLab == "discrete_short") {
                data <- data |>
                    dplyr::filter(
                        .data[[ivar]] %in% input[[paste0(ivar, "_menu")]]
                    )
            } else if (varLab == "discrete_long") {
                if (!is.null(reactive_values_2[[ivar]])) {
                    selected <- reactive_values_2[[ivar]]
                    if (length(selected) > 0) {
                        data <- data |>
                            dplyr::filter(
                                .data[[ivar]] %in% selected
                            )
                    }
                }
            } else if (varLab == "numeric") {
                shiny::req(input[[paste0(ivar, "_range")]])
                data <- data |>
                    dplyr::filter(
                        .data[[ivar]] >= input[[paste0(ivar, "_range")]][1],
                        .data[[ivar]] <= input[[paste0(ivar, "_range")]][2]
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
        visVars <- input$vars
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

    shiny::observe({
        visVars <- input$vars

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
        visVars <- input$vars
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
    
    output$plots_tab <- shiny::renderUI({
        shiny::req(input$vars)
        shiny::fluidRow(
            input$vars |> 
                purrr::map(\(.var) boxFun(.var, dat)) |>
                purrr::discard(is.null)
        )
    })
    
    shiny::observe({
        shiny::req(input$vars)
        res <- 150
        input$vars |> 
            purrr::map(\(.var) {
                varLab <- metadataVars[[.var]]$label
                if (varLab == "numeric") {
                    outputName <- make.names(paste0(.var, "_plot"))
                    output[[outputName]] <- shiny::renderPlot({
                        plotFun(filtered_data(), .var)
                    }, res = res)
                } else if (varLab == "discrete_short") {
                    outputName <- make.names(paste0(.var, "_plot"))
                    output[[outputName]] <- shiny::renderPlot({
                        plotFun(filtered_data(), .var)
                    }, res = res)
                } else if (varLab == "discrete_long") {
                    outputName <- make.names(paste0(.var, "_summary"))
                    output[[outputName]] <- shiny::renderUI({
                        counts <- reactive_list$list[[.var]]
                        summaryFun(.var, counts, reactive_values_2[[.var]])
                    })
                    return(NULL)
                }
            }) |>
            purrr::discard(is.null)
    })

    topBoxes(output, filtered_data)
    worldMap(output)
    fullTbl(input, output, filtered_data)

}

# Individual major output elements ----------------------------------------
topBoxes <- function(output, filtered_data) {
    ## TODO there could be some NA in filtered_data, especially in country. Double check.
    list(
        output$box_studies <- shinydashboard::renderValueBox({
            shinydashboard::valueBox(
                value = length(unique(filtered_data()$study_name)),
                subtitle = "Studies",
                icon = shiny::icon("flask"),
                color = "light-blue"
            )
        }), 
        output$box_samples <- shinydashboard::renderValueBox({
            shinydashboard::valueBox(
                value = format(length(unique(filtered_data()$sample_id)), big.mark = ","),
                subtitle = "Samples",
                icon = shiny::icon("chart-simple"),
                color = "yellow"
            )
        }),
        output$box_countries <- shinydashboard::renderValueBox({
            shinydashboard::valueBox(
                value = format(length(unique(filtered_data()$country)), big.mark = ","),
                subtitle = "Countries",
                icon = shiny::icon("earth-americas"),
                color = "teal"
            )
        }),
        output$box_diseases <- shinydashboard::renderValueBox({
            shinydashboard::valueBox(
                value = format(length(unique(filtered_data()$target_condition)), big.mark = ","),
                subtitle = "Conditions",
                icon = shiny::icon("heart-pulse"),
                color = "maroon"
            )
        })
    )
}

worldMap <- function(output) {
    output$map_tab <- leaflet::renderLeaflet({
        leaflet::leaflet() |>
            leaflet::addTiles(
                "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
                attribution = paste(
                    "&copy; <a href=\"https://openstreetmap.org\">OpenStreetMap</a> contributors",
                    "&copy; <a href=\"https://cartodb.com/attributions\">CartoDB</a>"
                ),
                options = leaflet::tileOptions(noWrap = TRUE)
            ) |>
            leaflet::setView(20, 40, zoom = 3)
    })
}

fullTbl <- function(input, output, filtered_data) {
    output$table_tab <- DT::renderDT({
        shiny::req(input$vars)
        DT::datatable(
            data = filtered_data(),
            rownames = FALSE,
            options = list(scrollX = TRUE)
        )
    })
}
