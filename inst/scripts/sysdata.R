library(purrr)
library(dplyr)

fname <- system.file(
    "extdata", "cMD_curated_metadata_release.csv",
    package = "cmdMeta", mustWork = TRUE
)
# dat <- read.csv(fname)
dat <- readr::read_csv(fname)
metadata <- dat |>
    mutate_at(.vars = c("pmid", "last_updated"), .funs = as.character) |>
    purrr::modify_if(
        .p = is.character,
        .f = function(x) {
            dplyr::case_when(
                is.na(x) ~ "NA",
                TRUE ~ x
            )
        }
    )

classVar <- map_chr(metadata, class)
lenVar <- map_int(metadata, ~ length(unique(.x[!is.na(.x)])))


# df <- data.frame(
#     names = names(dat),
#     class = unname(classVar),
#     len = unname(lenVar)
# )

metadataVars <- map2(classVar, lenVar, ~ {
    label <- dplyr::case_when(
        .x == "character" & .y <= 7 ~ "discrete_short",
        .x == "character" & .y > 7 ~ "disctete_long",
        TRUE ~ "numeric"
    )

    list(class = .x, length = .y, label = label)
})

usethis::use_data(metadataVars, metadata, overwrite = TRUE, internal = TRUE)
