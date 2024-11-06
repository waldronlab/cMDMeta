server <- function(input, output, session) {
    fname <- system.file(
        "extdata", "cMD_curated_metadata_release.csv",
        package = "cmdMeta", mustWork = TRUE
    )
    dat <- read.csv(fname)
    shiny::updateSelectizeInput(
        inputId = "studies",
        label = "Studies",
        choices = c("Select all", unique(dat$study_name))
    )

    p <- dat |>
        dplyr::count(.data$body_site) |>
        dplyr::mutate(
            per = .data$n / sum(.data$n) * 100
        ) |>
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = stats::reorder(.data$body_site, -.data$per),
                y = .data$per
            )
        ) +
        ggplot2::labs(
            x = "Body site", y = "Samples"
        ) +
        ggplot2::scale_y_continuous(labels = \(x) paste0(x, "%")) +
        ggplot2::geom_col() +
        ggplot2::theme_minimal()
    output$myPlot <- shiny::renderPlot({p}, res = 96)
}
