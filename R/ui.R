currentVars <- c(
    # "target_condition",
    # "control",
    # "country",
    "body_site",
    # "westernized",
    # "age_group",
    # "disease",
    # "sequencing_platform",
    # "sex",
    # "antibiotics_current_use",
    # "age_years",
    "bmi"
    # "treatment",
    # "smoker",
    # "ancestry"
)

createUI <- function() {
    ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(
            title = "cmdMeta"
        ),
        shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
                shinyWidgets::pickerInput(
                    inputId = "vars",
                    label = "Select variables",
                    choices = currentVars,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                ),
                shinydashboard::menuItem(
                    "Plots", tabName = "plots", icon = shiny::icon("th")
                ),
                shinydashboard::menuItem(
                    "Table", tabName = "table", icon = shiny::icon("file-text")
                )
            )
        ),
        shinydashboard::dashboardBody(
            htmltools::tags$head(
                htmltools::tags$style(htmltools::HTML("
                .fixed-height-box {
                    height: 400px;
                    display: flex;
                    flex-direction: column;
                }
                .plot-container {
                    flex: 0 0 300px;
                }
                .table-container {
                    flex: 0 0 250px;
                    overflow-y: auto;
                }
                .controls-container {
                    flex: 1;
                    display: flex;
                    flex-direction: column;
                    justify-content: space-between;
                }
                .summary-table {
                    width: 100%;
                    border-collapse: collapse;
                }
                .summary-table th, .summary-table td {
                    padding: 8px;
                    text-align: left;
                    border-bottom: 1px solid #ddd;
                }
                .summary-table th {
                    background-color: #f5f5f5;
                }
                .checkbox-cell {
                    text-align: center;
                }
            "))
            ),
            shinydashboard::tabItems(
                shinydashboard::tabItem(
                    tabName = "plots",
                    shiny::fluidRow(
                        shiny::uiOutput("dynamic_plots")
                    )
                ),
                shinydashboard::tabItem(
                    tabName = "table",
                    shiny::fluidRow(
                        shinydashboard::box(
                            title = "Table",
                            status = "info",
                            solidHeader = TRUE,
                            width = 12,
                            DT::DTOutput("filtered_table")
                        )
                    )
                )
            )
        )
    )
    return(ui)
}
