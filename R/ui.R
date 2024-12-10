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

noNAVars <- c(
    "body_site",
    # "control",
    # "country",
    "target_condition",
    # "westernized"
    "sex",
    "bmi"
)

createUI <- function() {
    ui <- shinydashboard::dashboardPage(
        skin = "black",
        title = "cmdMeta",
        header = shinydashboard::dashboardHeader(title = "Omics ML Repo"),
        sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE,
            shinydashboard::sidebarMenu(
                shinyWidgets::pickerInput(
                    inputId = "vars",
                    label = "Select variables",
                    choices = currentVars,
                    multiple = TRUE,
                    selected = currentVars[currentVars %in% noNAVars],
                    options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 1"
                    )
                ),
                shinydashboard::menuItem(
                    text = "Main", tabName = "plots",
                    icon = shiny::icon("house")
                ),
                shinydashboard::menuItem(
                    text = "Table", tabName = "table",
                    icon = shiny::icon("table")
                ),
                shinydashboard::menuItem(
                    text = "About", tabName = "about",
                    icon = shiny::icon("info-circle")
                )
            )
        ),
        body = shinydashboard::dashboardBody(
            htmltools::tags$head(
                htmltools::tags$style(htmltools::HTML(custom_css))
            ),
            shinydashboard::tabItems(
                shinydashboard::tabItem(
                    tabName = "plots",
                    shiny::fluidRow(
                        shinydashboard::valueBoxOutput(
                            outputId = "box_studies", width = 3
                        ),
                        shinydashboard::valueBoxOutput(
                            outputId = "box_samples", width = 3
                        ),
                        shinydashboard::valueBoxOutput(
                            outputId = "box_countries", width = 3
                        ),
                        shinydashboard::valueBoxOutput(
                            outputId = "box_diseases", width = 3
                        ),
                        shinydashboard::box(
                            leaflet::leafletOutput(outputId = "map_tab"),
                            width = 12
                        )
                    ),
                    shiny::uiOutput("plots_tab")
                ),
                shinydashboard::tabItem(
                    tabName = "table",
                    shiny::fluidPage(DT::DTOutput("table_tab"))
                )
            )
        )
    )
    return(ui)
}
