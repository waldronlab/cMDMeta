library(ggplot2)

fname <- system.file(
    "extdata", "cMD_curated_metadata_release.csv",
    package = "cmdMeta", mustWork = TRUE
)
dat <- read.csv(fname)

dat |>
    dplyr::count(body_site) |>
    ggplot(
        aes(reorder(body_site, -n), n)
    ) +
    geom_col()
