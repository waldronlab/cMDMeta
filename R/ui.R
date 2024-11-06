createUI <- function() {
    shiny::fluidPage(
        shiny::titlePanel("Options"),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::selectizeInput(
                    inputId = "studies",
                    label = "Studies",
                    choices = "",
                    multiple = TRUE
                )
            ),
            shiny::mainPanel(
                shiny::plotOutput(
                    outputId = "myPlot"
                )
            )
        )
    )
}
