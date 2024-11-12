
plotFun <- function(dat, varName) {
    varCls <- metadataVars[[varName]]$class
    varLen <- metadataVars[[varName]]$length
    varLab <- metadataVars[[varName]]$label
    var <- dat[[varName]]

    if (varCls == "character") {
        dat[[varName]] <- as.character(var)
        dat <- dat |>
            dplyr::filter(
                !is.na(.data[[varName]]) & .data[[varName]] != "NA"
            )
    } else if (varCls == "numeric" || varCls == "integer") {
        dat <- dat |>
            dplyr::filter(
                !is.na(.data[[varName]])
            )
    }

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
            ggplot2::geom_col(fill = "dodgerblue4") +
            # ggplot2::scale_x_discrete(labels= function(x) stringr::str_trunc(x, width = 10)) +
            ggplot2::labs(
                x = varName, y = "Samples"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
            )
        return(p)
    } else if (varLab == "numeric") {
        p <- dat |>
            ggplot2::ggplot(
                mapping = ggplot2::aes(
                    x = .data[[varName]]
                )
            ) +
            ggplot2::geom_histogram(
                fill = "dodgerblue4",
                color = "white", size = 0.1
            ) +
            ggplot2::theme_bw()
        return(p)
    }

}
