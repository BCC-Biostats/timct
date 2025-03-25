#' Calculate Average Pixel Intensity
#'
#' This function calculates the average intensity of pixels within a specified radius (or radii)
#' of each point. When given a vector of radii, it performs one spatial search using the maximum
#' radius and then filters the results for each point.
#'
#' @param points A data frame with columns `x` and `y` representing the spatial coordinates of points.
#' @param pixels A data frame with columns `x`, `y`, and `value` representing the spatial coordinates and intensity values of pixels.
#' @param radius A numeric value or a numeric vector. If a vector is provided, its length must equal the number of rows in `points`,
#'   with each element specifying the radius for the corresponding point.
#'
#' @return A numeric vector containing the average pixel intensity for each point in the `points` data frame.
#'
#' @importFrom dbscan frNN
#' @importFrom pbapply pbsapply
#' @examples
#' # Using a single radius
#' points <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3))
#' pixels <- data.frame(x = runif(100, 0, 5), y = runif(100, 0, 5), value = rnorm(100))
#' average_pixel_intensity(points, pixels, radius = 2)
#'
#' # Using a vector of radii
#' radius_vector <- c(1, 2, 3)
#' average_pixel_intensity(points, pixels, radius = radius_vector)
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

  # Check that radius is either a single number or a vector of the same length as number of points
  if (!(length(radius) == 1 || length(radius) == nrow(points))) {
    stop("Parameter 'radius' must be either a single numeric value or a vector with length equal to the number of points.")
  }

  # Load required packages for spatial search and progress bar
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package 'dbscan' is required. Please install it with install.packages('dbscan').")
  }
  if (!requireNamespace("pbapply", quietly = TRUE)) {
    stop("Package 'pbapply' is required. Please install it with install.packages('pbapply').")
  }

  # Convert the spatial coordinates to matrices
  query_coords <- as.matrix(points[, c("x", "y")])
  pixel_coords <- as.matrix(pixels[, c("x", "y")])

  if (length(radius) == 1) {
    # Single radius: use efficient spatial search with dbscan::frNN
    nn_results <- dbscan::frNN(pixel_coords, eps = radius, query = query_coords)
    means <- pbapply::pbsapply(nn_results$id, function(indices) {
      if (length(indices) == 0) {
        return(0)
      } else {
        return(mean(pixels$value[indices], na.rm = TRUE))
      }
    })
  } else {
    # Vector of radii: perform one search using the maximum radius and then filter for each point.
    max_radius <- max(radius)
    nn_results <- dbscan::frNN(pixel_coords, eps = max_radius, query = query_coords)
    means <- pbapply::pbsapply(seq_along(nn_results$id), function(i) {
      current_radius <- radius[i]
      # Retrieve neighbor indices and distances for the current point.
      indices <- nn_results$id[[i]]
      distances <- nn_results$dist[[i]]
      # Filter the indices based on the current point's radius.
      valid_indices <- indices[distances <= current_radius]
      if (length(valid_indices) == 0) {
        return(0)
      } else {
        return(mean(pixels$value[valid_indices], na.rm = TRUE))
      }
    })
  }

  return(means)
}
