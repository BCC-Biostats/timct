#' Manage FCS files into an all dataset
#'
#' @param path_to_files path to location of fcs files
#' @param path_to_csv path to location to write csv file
#'
#' @return file written to path_to_csv
#' @export
#'
#' @examples
#' fcs_mange()
#'
fcs_manage <- function(path_to_files = "data/raw_data/",
                       path_to_csv = "data/derived_data/") {

  files_list <- list.files(path = path_to_files,
                           pattern = "\\.fcs$",
                           full.names = TRUE)

  fcs_dat_list <- lapply(files_list, timct:::read_clean_fcs)

  fcs_dat <- fcs_dat_list |>
    dplyr::bind_rows()

  readr::write_csv(fcs_dat, paste0({{ path_to_csv }},
                                   "data_all.csv"))

}
