currentVars <- sort(c(
    "target_condition",
    "control",
    "country",
    "body_site",
    "westernized",
    "age_group",
    "disease",
    "sequencing_platform",
    "sex",
    "antibiotics_current_use",
    "age_years",
    "bmi",
    "treatment",
    "smoker",
    "ancestry"
))

createUI <- function() {
    ui <- shinydashboard::dashboardPage(
        skin = "blue",
        title = "cmdMeta",
        header = shinydashboard::dashboardHeader(title = "cmdMeta"),
        sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE,
            shinydashboard::sidebarMenu(
                shinyWidgets::pickerInput(
                    inputId = "vars",
                    label = "Select variables",
                    choices = currentVars,
                    multiple = TRUE,
                    selected = currentVars,
                    options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 1"
                    )
                ),
                shinydashboard::menuItem(
                    text = "Plots", tabName = "plots",
                    icon = shiny::icon("magnifying-glass-chart")
                ),
                shinydashboard::menuItem(
                    text = "Table", tabName = "table",
                    icon = shiny::icon("table")
                )
            )
        ),
        body = shinydashboard::dashboardBody(
            htmltools::tags$head(
                htmltools::tags$style(
                    htmltools::HTML(custom_css)
                )
            ),
            shinydashboard::tabItems(
                shinydashboard::tabItem(
                    tabName = "plots",
                    shiny::fluidRow(
                        shinydashboard::valueBoxOutput(
                            outputId = "box_studies", width = 6
                        ),
                        shinydashboard::valueBoxOutput(
                            outputId = "box_samples", width = 6
                        )
                    ),
                    # shiny::fluidRow(
                        shiny::uiOutput("plots_tab")
                    # )
                ),
                shinydashboard::tabItem(
                    tabName = "table",
                    shiny::fluidRow(
                        shinydashboard::box(
                            # title = "Table",
                            # status = "info",
                            solidHeader = TRUE,
                            width = 12,
                            DT::DTOutput("table_tab")
                        )
                    )
                )
            )
        )
    )
    return(ui)
}
