
plotFun <- function(dat, varName) {
    varCls <- metadataVars[[varName]]$class
    varLen <- metadataVars[[varName]]$length
    varLab <- metadataVars[[varName]]$label
    var <- dat[[varName]]

    if(!nrow(dat)) {
        p <- ggplot2::ggplot() +
            ggplot2::theme_void() +
            ggplot2::annotate("text", x = 0, y = 0, label = "No data available")
        return(p)
    }

    if (varLab == "discrete_short") {
        datSummary <- dat |>
            dplyr::count(.data[[varName]])
        p <- datSummary |>
            ggplot2::ggplot(
                mapping = ggplot2::aes(
                    x = stats::reorder(.data[[varName]], -.data[["n"]]),
                    y = .data[["n"]]
                )
            ) +
            ggplot2::geom_col() +
            ggplot2::labs(
                x = varName, y = "Samples"
            )
        return(p)
    } else if (varLab == "numeric") {
        p <- dat |>
            ggplot2::ggplot(
                mapping = ggplot2::aes(
                    x = .data[[varName]]
                )
            ) +
            ggplot2::geom_histogram()
        return(p)
    }

}


plotDiscrete <- function(dat) {}

# ggplot2::ggplot(
#     mapping = ggplot2::aes(x = )
# )
