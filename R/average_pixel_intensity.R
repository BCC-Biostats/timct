#' Calculate Average Pixel Intensity
#'
#' This function calculates the average intensity of pixels within a specified radius of each point.
#'
#' @param points A data frame with columns `x` and `y` representing the spatial coordinates of points.
#' @param pixels A data frame with columns `x`, `y`, and `value` representing the spatial coordinates and intensity values of pixels.
#' @param radius A numeric value specifying the radius within which pixel intensities will be averaged for each point.
#'
#' @return A numeric vector containing the average pixel intensity for each point in the `points` data frame.
#'
#' @importFrom dbscan frNN
#' @importFrom pbapply pbsapply
#' @examples
#' points <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3))
#' pixels <- data.frame(x = runif(100, 0, 5), y = runif(100, 0, 5), value = rnorm(100))
#' average_pixel_intensity(points, pixels, radius = 2)
#'
#' @export
average_pixel_intensity <- function(points, pixels, radius) {
  # Ensure required columns exist
  if (!all(c("x", "y") %in% colnames(points))) {
    stop("The 'points' data.frame must have 'x' and 'y' columns.")
  }
  if (!all(c("x", "y", "value") %in% colnames(pixels))) {
    stop("The 'pixels' data.frame must have 'x', 'y', and 'value' columns.")
  }

  # Load required packages for fast spatial search and progress bar
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package 'dbscan' is required. Please install it with install.packages('dbscan').")
  }
  if (!requireNamespace("pbapply", quietly = TRUE)) {
    stop("Package 'pbapply' is required. Please install it with install.packages('pbapply').")
  }

  # Convert the spatial coordinates to matrices
  query_coords <- as.matrix(points[, c("x", "y")])
  pixel_coords <- as.matrix(pixels[, c("x", "y")])

  # Find all pixels within the specified radius of each point.
  # dbscan::frNN uses an efficient spatial index (KD-tree-like)
  nn_results <- dbscan::frNN(pixel_coords, eps = radius, query = query_coords)

  # For each point, sum the 'value' from the pixels that fall within the radius.
  # Using pbapply::pbsapply provides a progress bar.
  means <- pbapply::pbsapply(nn_results$id, function(indices) {
    if (length(indices) == 0) {
      return(0)
    } else {
      return(mean(pixels$value[indices], na.rm = TRUE))
    }
  })

  return(means)
}
