#' Recursively Get Directory Structure
#'
#' This function returns either a nested list-of-lists representing the file and folder structure
#' or a flat data frame listing all files and directories under a specified path.
#'
#' When using the list output, if `full.names = FALSE` then each folder's contents are returned as relative names.
#' For the data frame output, a row is generated for every file or directory, with a column indicating whether it is a directory.
#'
#' @param path A character string specifying the root directory to start from. Defaults to the current working directory.
#' @param full.names Logical. If `TRUE`, return full paths; if `FALSE`, return relative paths. Defaults to `TRUE`.
#' @param return A character string indicating the type of output: `"list"` for a nested list or `"data.frame"` for a flat data frame.
#'   Defaults to `"list"`.
#'
#' @return Either a nested list (if `return = "list"`) or a data frame (if `return = "data.frame"`).
#'
#' @examples
#' # Get nested list structure with full paths (default)
#' get_dir_structure("my_folder")
#'
#' # Get nested list structure with relative paths
#' get_dir_structure("my_folder", full.names = FALSE)
#'
#' # Get data frame listing all files and directories with full paths
#' get_dir_structure("my_folder", return = "data.frame")
#'
#' @export
get_dir_structure <- function(path = ".", full.names = TRUE, return = c("list", "data.frame")) {
  return <- match.arg(return)

  if (return == "list") {
    # Use full.names parameter to get relative or full paths as requested
    items <- list.files(path, full.names = full.names)

    structure <- lapply(items, function(item) {
      # Compute full path for checking if it's a directory.
      item_full <- if (full.names) item else file.path(path, item)
      if (dir.exists(item_full)) {
        # Recursively get structure; note that we call with the full path so that recursion works properly.
        sub_structure <- get_dir_structure(item_full, full.names = full.names, return = "list")
        return(sub_structure)
      } else {
        return(item)
      }
    })

    # Set names based on the requested output style
    names(structure) <- if (full.names) basename(items) else items
    return(structure)
  }

  # For data.frame output, list files recursively using the same full.names argument
  all_items <- list.files(path, full.names = full.names, recursive = TRUE, include.dirs = TRUE)

  # For file.info, we need full paths; if full.names is FALSE, we construct them
  file_paths <- if (full.names) all_items else file.path(path, all_items)
  file_info <- file.info(file_paths)

  df <- data.frame(
    path = all_items,
    is_dir = file_info$isdir,
    stringsAsFactors = FALSE
  )
  return(df)
}
