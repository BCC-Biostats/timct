#' Read and CLean FCS Files
#'
#' @param file
#'
#' @return cleaned dataframe with fcs info
#'
#' @examples
#'
#' read_clean_fcs("path/to/file.fcs")
read_clean_fcs <- function(file) {

  fcs_dat <- flowCore::read.FCS(file) |>
    flowCore::exprs() |>
    as.data.frame() |>
    dplyr::mutate(ImageNumber = gsub(paste0("^", path_to_files, "|\\.fcs$"),
                              "",
                              file),
           MeanRadius = (MajorAxisLength + MinorAxisLength) / 2
    ) |>
    dplyr::select(-ImageId) |>
    dplyr::rename_with(~ ifelse(. == "CellId",
                                "ObjectNumber", .),
                       everything()) |>
    dplyr::select(ImageNumber, everything())

  return(fcs_dat)

}
