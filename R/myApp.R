
#' myApp
#'
#' @return A shiny app
#' @export
#'
myApp <- function() {
    shiny::shinyApp(ui = createUI(), server = server)
}
