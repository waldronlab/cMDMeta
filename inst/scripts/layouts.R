library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(dplyr)

# Define the fixed set of available cards
AVAILABLE_CARDS <- c(
    "Cylinder Count" = "A",
    "MPG Distribution" = "B",
    "Vehicle Class Summary" = "C"
)

ui <- dashboardPage(
    dashboardHeader(title = "Car Data Analysis"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Cards", tabName = "cards", icon = icon("th")),
            menuItem("Reports", tabName = "reports", icon = icon("file-text")),
            pickerInput(
                inputId = "selected_cards",
                label = "Select Cards to Display",
                choices = AVAILABLE_CARDS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
            )
        )
    ),
    
    dashboardBody(
        tags$head(
            tags$style(HTML("
                .fixed-height-box {
                    height: 400px;
                    display: flex;
                    flex-direction: column;
                }
                .plot-container {
                    flex: 0 0 250px;
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
        tabItems(
            tabItem(tabName = "cards",
                    fluidRow(
                        uiOutput("dynamic_cards")
                    )
            ),
            
            tabItem(tabName = "reports",
                    fluidRow(
                        box(
                            title = "Filtered Data Table",
                            status = "info",
                            solidHeader = TRUE,
                            width = 12,
                            DTOutput("filtered_table")
                        )
                    )
            )
        )
    )
)

server <- function(input, output, session) {
    visible_cards <- reactive({
        input$selected_cards
    })
    
    # Create a reactive for vehicle class counts
    class_counts <- reactive({
        data <- mpg
        counts <- table(data$class)
        data.frame(
            Class = names(counts),
            Count = as.numeric(counts),
            Percentage = sprintf("%.1f%%", 100 * as.numeric(counts) / sum(counts))
        ) %>%
            arrange(desc(Count))
    })
    
    # Reactive value to store selected classes
    selected_classes <- reactiveVal(unique(mpg$class))
    
    # Observer for class checkboxes
    observe({
        class_list <- unique(mpg$class)
        lapply(class_list, function(cls) {
            if (!is.null(input[[paste0("class_", make.names(cls))]])) {
                observeEvent(input[[paste0("class_", make.names(cls))]], {
                    current <- selected_classes()
                    if (input[[paste0("class_", make.names(cls))]]) {
                        selected_classes(unique(c(current, cls)))
                    } else {
                        selected_classes(setdiff(current, cls))
                    }
                })
            }
        })
    })
    
    filtered_data <- reactive({
        data <- mtcars
        data$car_name <- rownames(mtcars)
        
        if (is.null(input$selected_cyls) || length(input$selected_cyls) == 0) {
            return(data[0,])
        }
        
        data <- data[data$cyl %in% input$selected_cyls,]
        
        if (!is.null(input$mpg_range)) {
            data <- data[data$mpg >= input$mpg_range[1] & data$mpg <= input$mpg_range[2],]
        }
        
        data$cyl <- as.factor(data$cyl)
        data <- data[, c("car_name", setdiff(names(data), "car_name"))]
        
        return(data)
    })
    
    generate_cyl_plot <- function() {
        data <- filtered_data()
        if(nrow(data) == 0) {
            return(ggplot() + 
                       theme_void() + 
                       annotate("text", x = 0, y = 0, label = "No data available"))
        }
        
        ggplot(data, aes(x = factor(cyl))) +
            geom_bar(fill = "lightblue") +
            theme_minimal() +
            labs(
                x = "Number of Cylinders",
                y = "Count"
            )
    }
    
    generate_mpg_plot <- function() {
        data <- filtered_data()
        if(nrow(data) == 0) {
            return(ggplot() + 
                       theme_void() + 
                       annotate("text", x = 0, y = 0, label = "No data available"))
        }
        
        ggplot(data, aes(x = mpg)) +
            geom_histogram(binwidth = 2, fill = "lightblue", color = "white") +
            theme_minimal() +
            labs(
                x = "Miles Per Gallon",
                y = "Count"
            )
    }
    
    observe({
        card_list <- visible_cards()
        lapply(card_list, function(id) {
            button_id <- paste0("remove_", id)
            observeEvent(input[[button_id]], {
                current_selection <- input$selected_cards
                new_selection <- setdiff(current_selection, id)
                updatePickerInput(session, "selected_cards", selected = new_selection)
            }, ignoreInit = TRUE)
        })
    })
    
    output$filtered_table <- renderDT({
        data <- filtered_data()
        datatable(data,
                  options = list(
                      pageLength = 10,
                      scrollX = TRUE,
                      dom = 'Bfrtip'
                  ),
                  rownames = FALSE,
                  caption = "Car Performance Data"
        )
    })
    
    # Create the class summary table output with checkboxes
    output$class_summary <- renderUI({
        counts_df <- class_counts()
        
        tags$table(
            class = "summary-table",
            tags$thead(
                tags$tr(
                    tags$th("Select"),
                    tags$th("Vehicle Class"),
                    tags$th("Count"),
                    tags$th("Percentage")
                )
            ),
            tags$tbody(
                lapply(1:nrow(counts_df), function(i) {
                    cls <- counts_df$Class[i]
                    tags$tr(
                        tags$td(
                            class = "checkbox-cell",
                            checkboxInput(
                                inputId = paste0("class_", make.names(cls)),
                                label = NULL,
                                value = cls %in% selected_classes()
                            )
                        ),
                        tags$td(cls),
                        tags$td(counts_df$Count[i]),
                        tags$td(counts_df$Percentage[i])
                    )
                })
            )
        )
    })
    
    output$dynamic_cards <- renderUI({
        card_list <- visible_cards()
        
        if (length(card_list) == 0) return(NULL)
        
        fluidRow(
            lapply(card_list, function(id) {
                if (id == "A") {  # Cylinder count card
                    column(
                        width = 4,
                        box(
                            width = NULL,
                            title = "Cylinder Distribution",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            class = "fixed-height-box",
                            div(class = "plot-container",
                                plotOutput("cyl_plot", height = "100%")
                            ),
                            div(class = "controls-container",
                                pickerInput(
                                    inputId = "selected_cyls",
                                    label = NULL,
                                    choices = sort(unique(mtcars$cyl)),
                                    multiple = TRUE,
                                    selected = sort(unique(mtcars$cyl)),
                                    options = list(`actions-box` = TRUE)
                                ),
                                div(
                                    style = "text-align: right;",
                                    actionButton("remove_A", "Remove", class = "btn-danger btn-sm")
                                )
                            )
                        )
                    )
                } else if (id == "B") {  # MPG histogram card
                    column(
                        width = 4,
                        box(
                            width = NULL,
                            title = "MPG Distribution",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            class = "fixed-height-box",
                            div(class = "plot-container",
                                plotOutput("mpg_plot", height = "100%")
                            ),
                            div(class = "controls-container",
                                sliderInput(
                                    inputId = "mpg_range",
                                    label = NULL,
                                    min = floor(min(mtcars$mpg)),
                                    max = ceiling(max(mtcars$mpg)),
                                    value = c(floor(min(mtcars$mpg)), ceiling(max(mtcars$mpg))),
                                    step = 1
                                ),
                                div(
                                    style = "text-align: right;",
                                    actionButton("remove_B", "Remove", class = "btn-danger btn-sm")
                                )
                            )
                        )
                    )
                } else if (id == "C") {  # Vehicle class summary card
                    column(
                        width = 4,
                        box(
                            width = NULL,
                            title = "Vehicle Class Distribution",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            class = "fixed-height-box",
                            div(class = "table-container",
                                uiOutput("class_summary")
                            ),
                            div(class = "controls-container",
                                div(
                                    style = "text-align: right;",
                                    actionButton("remove_C", "Remove", class = "btn-danger btn-sm")
                                )
                            )
                        )
                    )
                }
            })
        )
    })
    
    output$cyl_plot <- renderPlot({
        generate_cyl_plot()
    })
    
    output$mpg_plot <- renderPlot({
        generate_mpg_plot()
    })
}

shinyApp(ui, server)


# library(shiny)
# library(shinydashboard)
# library(shinyWidgets)
# library(ggplot2)
# library(DT)
# library(dplyr)
# 
# # Define the fixed set of available cards
# AVAILABLE_CARDS <- c(
#     "Cylinder Count" = "A",
#     "MPG Distribution" = "B",
#     "Vehicle Class Summary" = "C"
# )
# 
# ui <- dashboardPage(
#     dashboardHeader(title = "Car Data Analysis"),
#     
#     dashboardSidebar(
#         sidebarMenu(
#             menuItem("Cards", tabName = "cards", icon = icon("th")),
#             menuItem("Reports", tabName = "reports", icon = icon("file-text")),
#             pickerInput(
#                 inputId = "selected_cards",
#                 label = "Select Cards to Display",
#                 choices = AVAILABLE_CARDS,
#                 multiple = TRUE,
#                 options = list(`actions-box` = TRUE)
#             )
#         )
#     ),
#     
#     dashboardBody(
#         tags$head(
#             tags$style(HTML("
#                 .fixed-height-box {
#                     height: 400px;
#                     display: flex;
#                     flex-direction: column;
#                 }
#                 .plot-container {
#                     flex: 0 0 250px;
#                 }
#                 .table-container {
#                     flex: 0 0 250px;
#                     overflow-y: auto;
#                 }
#                 .controls-container {
#                     flex: 1;
#                     display: flex;
#                     flex-direction: column;
#                     justify-content: space-between;
#                 }
#                 .summary-table {
#                     width: 100%;
#                     border-collapse: collapse;
#                 }
#                 .summary-table th, .summary-table td {
#                     padding: 8px;
#                     text-align: left;
#                     border-bottom: 1px solid #ddd;
#                 }
#                 .summary-table th {
#                     background-color: #f5f5f5;
#                 }
#                 .checkbox-cell {
#                     text-align: center;
#                 }
#             "))
#         ),
#         tabItems(
#             tabItem(tabName = "cards",
#                     fluidRow(
#                         uiOutput("dynamic_cards")
#                     )
#             ),
#             
#             tabItem(tabName = "reports",
#                     fluidRow(
#                         box(
#                             title = "Filtered Data Table",
#                             status = "info",
#                             solidHeader = TRUE,
#                             width = 12,
#                             DTOutput("filtered_table")
#                         )
#                     )
#             )
#         )
#     )
# )
# 
# server <- function(input, output, session) {
#     visible_cards <- reactive({
#         input$selected_cards
#     })
#     
#     # Create a reactive for vehicle class counts
#     class_counts <- reactive({
#         data <- mpg
#         counts <- table(data$class)
#         data.frame(
#             Class = names(counts),
#             Count = as.numeric(counts),
#             Percentage = sprintf("%.1f%%", 100 * as.numeric(counts) / sum(counts))
#         ) %>%
#             arrange(desc(Count))
#     })
#     
#     # Reactive value to store selected classes
#     selected_classes <- reactiveVal(unique(mpg$class))
#     
#     # Observer for class checkboxes
#     observe({
#         class_list <- unique(mpg$class)
#         lapply(class_list, function(cls) {
#             if (!is.null(input[[paste0("class_", make.names(cls))]])) {
#                 observeEvent(input[[paste0("class_", make.names(cls))]], {
#                     current <- selected_classes()
#                     if (input[[paste0("class_", make.names(cls))]]) {
#                         selected_classes(unique(c(current, cls)))
#                     } else {
#                         selected_classes(setdiff(current, cls))
#                     }
#                 })
#             }
#         })
#     })
#     
#     filtered_data <- reactive({
#         data <- mtcars
#         data$car_name <- rownames(mtcars)
#         
#         if (is.null(input$selected_cyls) || length(input$selected_cyls) == 0) {
#             return(data[0,])
#         }
#         
#         data <- data[data$cyl %in% input$selected_cyls,]
#         
#         if (!is.null(input$mpg_range)) {
#             data <- data[data$mpg >= input$mpg_range[1] & data$mpg <= input$mpg_range[2],]
#         }
#         
#         data$cyl <- as.factor(data$cyl)
#         data <- data[, c("car_name", setdiff(names(data), "car_name"))]
#         
#         return(data)
#     })
#     
#     generate_cyl_plot <- function() {
#         data <- filtered_data()
#         if(nrow(data) == 0) {
#             return(ggplot() + 
#                        theme_void() + 
#                        annotate("text", x = 0, y = 0, label = "No data available"))
#         }
#         
#         ggplot(data, aes(x = factor(cyl))) +
#             geom_bar(fill = "lightblue") +
#             theme_minimal() +
#             labs(
#                 x = "Number of Cylinders",
#                 y = "Count"
#             )
#     }
#     
#     generate_mpg_plot <- function() {
#         data <- filtered_data()
#         if(nrow(data) == 0) {
#             return(ggplot() + 
#                        theme_void() + 
#                        annotate("text", x = 0, y = 0, label = "No data available"))
#         }
#         
#         ggplot(data, aes(x = mpg)) +
#             geom_histogram(binwidth = 2, fill = "lightblue", color = "white") +
#             theme_minimal() +
#             labs(
#                 x = "Miles Per Gallon",
#                 y = "Count"
#             )
#     }
#     
#     observe({
#         card_list <- visible_cards()
#         lapply(card_list, function(id) {
#             button_id <- paste0("remove_", id)
#             observeEvent(input[[button_id]], {
#                 current_selection <- input$selected_cards
#                 new_selection <- setdiff(current_selection, id)
#                 updatePickerInput(session, "selected_cards", selected = new_selection)
#             }, ignoreInit = TRUE)
#         })
#     })
#     
#     output$filtered_table <- renderDT({
#         data <- filtered_data()
#         datatable(data,
#                   options = list(
#                       pageLength = 10,
#                       scrollX = TRUE,
#                       dom = 'Bfrtip'
#                   ),
#                   rownames = FALSE,
#                   caption = "Car Performance Data"
#         )
#     })
#     
#     # Create the class summary table output with checkboxes
#     output$class_summary <- renderUI({
#         counts_df <- class_counts()
#         
#         tags$table(
#             class = "summary-table",
#             tags$thead(
#                 tags$tr(
#                     tags$th("Select"),
#                     tags$th("Vehicle Class"),
#                     tags$th("Count"),
#                     tags$th("Percentage")
#                 )
#             ),
#             tags$tbody(
#                 lapply(1:nrow(counts_df), function(i) {
#                     cls <- counts_df$Class[i]
#                     tags$tr(
#                         tags$td(
#                             class = "checkbox-cell",
#                             checkboxInput(
#                                 inputId = paste0("class_", make.names(cls)),
#                                 label = NULL,
#                                 value = cls %in% selected_classes()
#                             )
#                         ),
#                         tags$td(cls),
#                         tags$td(counts_df$Count[i]),
#                         tags$td(counts_df$Percentage[i])
#                     )
#                 })
#             )
#         )
#     })
#     
#     output$dynamic_cards <- renderUI({
#         card_list <- visible_cards()
#         
#         if (length(card_list) == 0) return(NULL)
#         
#         fluidRow(
#             lapply(card_list, function(id) {
#                 if (id == "A") {  # Cylinder count card
#                     column(
#                         width = 4,
#                         box(
#                             width = NULL,
#                             title = "Cylinder Distribution",
#                             status = "primary",
#                             solidHeader = TRUE,
#                             collapsible = TRUE,
#                             class = "fixed-height-box",
#                             div(class = "plot-container",
#                                 plotOutput("cyl_plot", height = "100%")
#                             ),
#                             div(class = "controls-container",
#                                 pickerInput(
#                                     inputId = "selected_cyls",
#                                     label = NULL,
#                                     choices = sort(unique(mtcars$cyl)),
#                                     multiple = TRUE,
#                                     selected = sort(unique(mtcars$cyl)),
#                                     options = list(`actions-box` = TRUE)
#                                 ),
#                                 div(
#                                     style = "text-align: right;",
#                                     actionButton("remove_A", "Remove", class = "btn-danger btn-sm")
#                                 )
#                             )
#                         )
#                     )
#                 } else if (id == "B") {  # MPG histogram card
#                     column(
#                         width = 4,
#                         box(
#                             width = NULL,
#                             title = "MPG Distribution",
#                             status = "primary",
#                             solidHeader = TRUE,
#                             collapsible = TRUE,
#                             class = "fixed-height-box",
#                             div(class = "plot-container",
#                                 plotOutput("mpg_plot", height = "100%")
#                             ),
#                             div(class = "controls-container",
#                                 sliderInput(
#                                     inputId = "mpg_range",
#                                     label = NULL,
#                                     min = floor(min(mtcars$mpg)),
#                                     max = ceiling(max(mtcars$mpg)),
#                                     value = c(floor(min(mtcars$mpg)), ceiling(max(mtcars$mpg))),
#                                     step = 1
#                                 ),
#                                 div(
#                                     style = "text-align: right;",
#                                     actionButton("remove_B", "Remove", class = "btn-danger btn-sm")
#                                 )
#                             )
#                         )
#                     )
#                 } else if (id == "C") {  # Vehicle class summary card
#                     column(
#                         width = 4,
#                         box(
#                             width = NULL,
#                             title = "Vehicle Class Distribution",
#                             status = "primary",
#                             solidHeader = TRUE,
#                             collapsible = TRUE,
#                             class = "fixed-height-box",
#                             div(class = "table-container",
#                                 uiOutput("class_summary")
#                             ),
#                             div(class = "controls-container",
#                                 div(
#                                     style = "text-align: right;",
#                                     actionButton("remove_C", "Remove", class = "btn-danger btn-sm")
#                                 )
#                             )
#                         )
#                     )
#                 }
#             })
#         )
#     })
#     
#     output$cyl_plot <- renderPlot({
#         generate_cyl_plot()
#     })
#     
#     output$mpg_plot <- renderPlot({
#         generate_mpg_plot()
#     })
# }
# 
# shinyApp(ui, server)