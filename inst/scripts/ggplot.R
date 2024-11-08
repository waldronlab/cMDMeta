# library(ggplot2)
library(purrr)

fname <- system.file(
    "extdata", "cMD_curated_metadata_release.csv",
    package = "cmdMeta", mustWork = TRUE
)
dat <- read.csv(fname)

# dat |>
#     dplyr::count(sex) |>
#     ggplot(
#         aes(reorder(sex, -n), n)
#     ) +
#     geom_col()


map_chr(dat, class)
map_int(dat, ~ {
    length(unique(.x[!is.na(.x)]))
})
