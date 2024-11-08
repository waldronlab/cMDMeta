# library(ggplot2)
library(purrr)
library(dplyr)

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


classVar <- map_chr(dat, class)
lenVar <- map_int(dat, ~ {
    length(unique(.x[!is.na(.x)]))
})

df <- data.frame(
    names = names(dat),
    class = unname(classVar),
    length = unname(lenVar)
)


df |>
    arrange(class, -length) |>
    View()

l <- map2(classVar, lenVar, ~ list(class = .x, length = .y))




plotFun(dat, "body_site")
plotFun(dat, "bmi")
plotFun(dat, "country")



