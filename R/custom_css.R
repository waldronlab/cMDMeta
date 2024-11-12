# custom_css <- "
#     .fixed-height-box {
#         height: 450px;
#         display: flex;
#         flex-direction: column;
#     }
#
#     .plot-container {
#         flex: 0 0 300px;
#     }
#
#     .table-container {
#         flex: 0 0 400px;
#         overflow-y: auto;
#     }
#
#     .controls-container {
#         flex: 1;
#         display: flex;
#         flex-direction: column;
#         justify-content: space-between;
#     }
#
#     .summary-table {
#         width: 100%;
#         border-collapse: collapse;
#     }
#
#     .summary-table th, .summary-table td {
#         padding: 8px;
#         text-align: left;
#         border-bottom: 1px solid #ddd;
#     }
#
#     .summary-table th {
#         background-color: #f5f5f5;
#     }
#
#     .checkbox-cell {
#         text-align: center;
#     }
# "

custom_css <- system.file(
    "www", "custom.css", package = "cmdMeta"
) |>
    readLines() |>
    paste(collapse = "\n")

# paste(readLines(fn), collapse = "\n")


