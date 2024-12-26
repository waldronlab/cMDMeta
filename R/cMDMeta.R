
#' cMDMeta
#'
#' @return A shiny app
#' @export
#'
cMDMeta <- function() {
    shiny::shinyApp(ui = createUI(), server = server)
}
