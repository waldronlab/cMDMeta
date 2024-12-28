
# Plot "continuous" and "discrete-short" variables ------------------------
plotFun <- function(dat, var) {
    
    varLab <- metadataVars[[var]]$label
    
    if (varLab == "discrete_short" || varLab == "discrete_long") {
        dat[[var]] <- as.character(dat[[var]])
        dat <- dplyr::filter(dat, !is.na(.data[[var]]) & .data[[var]] != "NA")
    } else if (varLab == "numeric") {
        dat <- dplyr::filter(dat, !is.na(.data[[var]]))
    }
    
    if(!nrow(dat)) { 
        p <- ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::annotate(
                "text", x = 0, y = 0, label = "No data available."
            )
        return(p)
    }
    
    if (varLab == "discrete_short") {
        cols <- grDevices::palette.colors(8)
        cols <- cols[2:length(cols)]
        datSummary <- dplyr::count(dat, .data[[var]])
        p <- datSummary |>
            ggplot2::ggplot(
                mapping = ggplot2::aes(
                    x = "",
                    y = .data[["n"]],
                    fill = .data[[var]]
                )
            ) +
            ggplot2::geom_col() +
            ggplot2::scale_fill_manual(values = cols) +
            ggplot2::labs(x = var, y = "Samples") +
            ggplot2::coord_polar("y", start = 0, direction = -1) +
            ggplot2::theme_void() +
            ggplot2::theme(legend.position = "none")
    } else if (varLab == "numeric") {
        p <- dat |>
            ggplot2::ggplot(
                mapping = ggplot2::aes(x = .data[[var]])
            ) +
            ggplot2::geom_histogram(
                fill = "dodgerblue4",
                color = "white", size = 0.1
            ) +
            ggplot2::theme_bw()
    }
    return(p)
}

# Summary table "discrete-long" variables ---------------------------------
summaryFun <- function(x, counts, selected_values) {
    # message("Here i am")
    htmltools::tags$table(
        class = "summary-table",
        htmltools::tags$thead(
            htmltools::tags$tr(
                htmltools::tags$th("Select"),
                htmltools::tags$th(x),
                htmltools::tags$th("n"),
                htmltools::tags$th("%")
            )
        ),
        htmltools::tags$body(
            purrr::map(1:nrow(counts), function(i) {
                cls <- counts[[x]][i]
                htmltools::tags$tr(
                    htmltools::tags$td(
                        class = "checkbox-cell",
                        shiny::checkboxInput(
                            inputId = paste0("class_", x, "_", make.names(cls)),
                            label = NULL,
                            value = cls %in% selected_values
                        )
                    ),
                    htmltools::tags$td(cls),
                    htmltools::tags$td(counts$n[i]),
                    htmltools::tags$td(counts$per[i]),
                )
            })
        )
    )
}



# render UI ---------------------------------------------------------------
boxDiscreteShort <- function(var, dat) {
    plotOutputId <- make.names(paste0(var, "_plot"))
    widgetId <- make.names(paste0(var, "_menu"))
    shinydashboard::box(
        width = 4,
        title = .textR(var),
        solidHeader = TRUE,
        # class = "fixed-height-box",
        shiny::plotOutput(
            plotOutputId
        ),
        shinyWidgets::pickerInput(
            inputId = widgetId,
            label = NULL,
            choices = sort(unique(dat[[var]])),
            multiple = TRUE,
            selected = sort(unique(dat[[var]])),
            options = list(
                `actions-box` = TRUE,
                `live-search` = FALSE,
                `selected-text-format` = "count > 1"
            )
        )
    )
}

boxDiscreteLong <- function(var) {
    outputId <- make.names(paste0(var, "_", "summary"))
        shinydashboard::box(
            width = 4,
            title = .textR(var),
            solidHeader = TRUE,
            class = "fixed-height-box",
            htmltools::div(
            class = "table-container",
                shiny::uiOutput(outputId)
            )
        )
}

boxContinuous <- function(var, dat) {
    plotOutputId <- make.names(paste0(var, "_plot"))
    widgetId <- make.names(paste0(var, "_range"))
    shinydashboard::box(
        width = 4,
        title = .textR(var),
        solidHeader = TRUE,
        class = "fixed-height-box",
        shiny::plotOutput(plotOutputId),
        shiny::sliderInput(
            inputId = widgetId,
            label = NULL,
            min = floor(min(dat[[var]], na.rm = TRUE)),
            max = ceiling(max(dat[[var]], na.rm = TRUE)),
            value = c(floor(min(dat[[var]], na.rm = TRUE)), ceiling(max(dat[[var]], na.rm = TRUE))),
            step = 1
        )
    )
}

boxFun <- function(var, dat) {
    varLab <- metadataVars[[var]]$label
    if (varLab == "numeric") {
        return(boxContinuous(var, dat))
    } else if (varLab == "discrete_short") {
        return(boxDiscreteShort(var, dat))
    } else if (varLab == "discrete_long") {
        return(boxDiscreteLong(var))
    }
}

.textR <- function(var) {
    var |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_to_sentence()
}
