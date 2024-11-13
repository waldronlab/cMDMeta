library(shiny)
reactiveConsole(TRUE)

temp_c <- reactiveVal(10) # create
temp_c()                  # get
#> [1] 10
temp_c(20)                # set
temp_c()                  # get
#> [1] 20


temp_f <- reactive({
    message("Converting")
    (temp_c() * 9 / 5) + 32
})
temp_f()
temp_c(100)
temp_f()
