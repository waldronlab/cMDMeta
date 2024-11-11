summaryFun <- function(x, counts, selected_values) {
    htmltools::tags$table(
        class = "summary-table",
        htmltools::tags$thead(
            htmltools::tags$tr(
                htmltools::tags$th("Select"),
                htmltools::tags$th(x),
                htmltools::tags$th("n"),
                htmltools::tags$th("%")
            )
        ),
        htmltools::tags$body(
            purrr::map(1:nrow(counts), function(i) {
                cls <- counts[[x]][i]
                htmltools::tags$tr(
                    htmltools::tags$td(
                        class = "checkbox-cell",
                        shiny::checkboxInput(
                            inputId = paste0("class_", x, "_", make.names(cls)),
                            label = NULL,
                            value = cls %in% selected_values
                        )
                    ),
                    htmltools::tags$td(cls),
                    htmltools::tags$td(counts$n[i]),
                    htmltools::tags$td(counts$per[i]),
                )
            })
        )
    )
}
