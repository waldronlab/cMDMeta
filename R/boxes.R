boxDiscreteShort <- function(x, dat) {
    plotOutputId <- make.names(paste0(x, "_plot"))
    widgetId <- make.names(paste0(x, "_menu"))
    shiny::column(
        width = 3,
        shinydashboard::box(
            width = NULL,
            title = x,
            solidHeader = TRUE,
            class = "fixed-height-box",
            htmltools::div(
                class = "plot-container",
                shiny::plotOutput(plotOutputId, height = "100%")
            ),
            htmltools::div(
                class = "controls-container",
                shinyWidgets::pickerInput(
                    inputId = widgetId,
                    label = NULL,
                    choices = sort(unique(dat[[x]])),
                    multiple = TRUE,
                    selected = sort(unique(dat[[x]])),
                    options = list(
                        `actions-box` = TRUE,
                        `live-search` = FALSE,
                        `selected-text-format` = "count > 1"
                    )
                )
            )
        )
    )
}

boxDiscreteLong <- function(x) {
    outputId <- make.names(paste0(x, "_", "summary"))
    shiny::column(
        width = 3,
        shinydashboard::box(
            width = NULL,
            title = x,
            solidHeader = TRUE,
            class = "fixed-height-box",
            htmltools::div(
                class = "table-container",
                shiny::uiOutput(outputId)
            )
        )
    )
}

boxContinuous <- function(x, dat) {
    plotOutputId <- make.names(paste0(x, "_plot"))
    widgetId <- make.names(paste0(x, "_range"))
    shiny::column(
        width = 3,
        shinydashboard::box(
            width = NULL,
            title = x,
            status = NULL,
            solidHeader = TRUE,
            class = "fixed-height-box",
            htmltools::div(
                class = "plot-container",
                shiny::plotOutput(plotOutputId, height = "100%")
            ),
            htmltools::div(
                class = "controls-container",
                shiny::sliderInput(
                    inputId = widgetId,
                    label = NULL,
                    min = floor(min(dat[[x]])),
                    max = ceiling(max(dat[[x]])),
                    value = c(floor(min(dat[[x]])), ceiling(max(dat[[x]]))),
                    step = 1
                )
            )
        )
    )
}

boxFun <- function(x, dat) {
    classVar <- metadataVars[[x]]$class
    lenVar <- metadataVars[[x]]$length
    if (classVar == "numeric" || classVar == "integer") {
        return(boxContinuous(x, dat))
    } else if (classVar == "character" && lenVar <= 7) {
        return(boxDiscreteShort(x, dat))
    } else if (classVar == "character" && lenVar > 7) {
        return(boxDiscreteLong(x))
    }
}
