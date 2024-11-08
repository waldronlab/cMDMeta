library(purrr)
library(dplyr)

fname <- system.file(
    "extdata", "cMD_curated_metadata_release.csv",
    package = "cmdMeta", mustWork = TRUE
)
dat <- read.csv(fname) |>
    ## some integers should be character (discrete)
    mutate_at(.vars = "pmid", .funs = as.character)

classVar <- map_chr(dat, class)
lenVar <- map_int(dat, ~ length(unique(.x[!is.na(.x)])))


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



usethis::use_data(metadataVars, overwrite = TRUE, internal = TRUE)
