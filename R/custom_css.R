
custom_css <- system.file(
    "www", "custom.css", package = "cmdMeta"
) |>
    readLines() |>
    paste(collapse = "\n")
