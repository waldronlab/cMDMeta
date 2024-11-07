createUI <- function() {
    shiny::fluidPage(
        shiny::titlePanel("cMDMeta"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shinyWidgets::pickerInput(
                    inputId = "studies",
                    label = "Studies",
                    choices = "",
                    multiple = TRUE,
                    inline =  "fit",
                    options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 1"
                    )
                )
            ),
            # shiny::mainPanel(shiny::plotOutput(outputId = "myPlot"))
            shiny::mainPanel(plotly::plotlyOutput(outputId = "myPlot"))
        )
    )
}

server <- function(input, output, session) {
    fname <- system.file(
        "extdata", "cMD_curated_metadata_release.csv",
        package = "cmdMeta", mustWork = TRUE
    )
    dat <- read.csv(fname)
    shinyWidgets::updatePickerInput(
        inputId = "studies",
        label = paste0("Studies (", length(unique(dat$study_name)), " available )"),
        choices = unique(dat$study_name),
        selected = unique(dat$study_name)
    )
    datReactive <- reactive({
        dat |>
            dplyr::filter(.data$study_name %in% input$studies)
    })
    # output$myPlot <- shiny::renderPlot({
    output$myPlot <- plotly::renderPlotly({
        p <- datReactive() |>
            dplyr::mutate(
                sex = ifelse(is.na(.data$sex), "NA", .data$sex)
            ) |>
            dplyr::count(.data$sex) |>
            dplyr::mutate(
                percentage = ceiling(.data$n / sum(.data$n) * 100)
            ) |>
            ggplot2::ggplot(
                mapping = ggplot2::aes(
                    x = stats::reorder(.data$sex, -.data$percentage),
                    y = .data$percentage
                )
            ) +
            ggplot2::labs(
                x = "Body site", y = "Samples (%)"
            ) +
            # ggplot2::scale_y_continuous(labels = \(x) paste0(x, "%")) +
            ggplot2::geom_col(fill = "dodgerblue3") +
            ggplot2::theme_bw()
        plotly::ggplotly(
            p = p, tooltip = "y"
        )
    })
    # }, res = 96)
}

myApp <- function() {
    shiny::shinyApp(ui = createUI(), server = server)
}
