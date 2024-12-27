boxDiscreteShort <- function(x, dat) {
    plotOutputId <- make.names(paste0(x, "_plot"))
    widgetId <- make.names(paste0(x, "_menu"))
    shinydashboard::box(
        width = 4,
        title = .textR(x),
        solidHeader = TRUE,
        # class = "fixed-height-box",
        shiny::plotOutput(
            plotOutputId
        ),
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
}

# boxDiscreteLong <- function(x) {
#     outputId <- make.names(paste0(x, "_", "summary"))
#         shinydashboard::box(
#             width = 4,
#             title = .textR(x),
#             solidHeader = TRUE,
#             class = "fixed-height-box",
#             htmltools::div(
#             class = "table-container",
#                 shiny::uiOutput(outputId)
#             )
#         )
# }

boxContinuous <- function(x, dat) {
    plotOutputId <- make.names(paste0(x, "_plot"))
    widgetId <- make.names(paste0(x, "_range"))
    shinydashboard::box(
        width = 4,
        title = .textR(x),
        solidHeader = TRUE,
        # class = "fixed-height-box",
        shiny::plotOutput(plotOutputId),
        shiny::sliderInput(
            inputId = widgetId,
            label = NULL,
            min = floor(min(dat[[x]], na.rm = TRUE)),
            max = ceiling(max(dat[[x]], na.rm = TRUE)),
            value = c(floor(min(dat[[x]], na.rm = TRUE)), ceiling(max(dat[[x]], na.rm = TRUE))),
            step = 1
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

.textR <- function(x) {
    x |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_to_sentence()
}
