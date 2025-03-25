#' Fit a Kernel Regression Surface and Predict Cell Values Using np Package
#'
#' This function fits a nonparametric regression surface to pixel data using a Gaussian kernel
#' via the np package. It then predicts the z value for each cell provided by interpolating
#' the fitted surface. The np package efficiently computes the kernel regression.
#'
#' @param cells A data frame with columns `x` and `y` representing the coordinates of the cells.
#' @param pixels A data frame with columns `x`, `y`, and `value` representing the pixel coordinates and intensities.
#' @param bw Optional bandwidth object. If not provided, one will be selected automatically using npregbw.
#'
#' @return A numeric vector of predicted surface values (z) corresponding to each cell.
#'
#' @import np
#' @examples
#' \dontrun{
#' # Ensure the np package is installed: install.packages("np")
#' library(np)
#' set.seed(123)
#' # Generate example pixel data
#' pixels <- data.frame(
#'   x = runif(100, 0, 5),
#'   y = runif(100, 0, 5),
#'   value = rnorm(100)
#' )
#' # Define cell locations where predictions are desired
#' cells <- data.frame(
#'   x = c(1, 2, 3),
#'   y = c(1, 2, 3)
#' )
#' # Fit the surface and predict cell values
#' preds <- predict_surface_cells_np(cells, pixels)
#' print(preds)
#' }
#'
#' @export
pixel_surface_values <- function(cells, pixels, bw = NULL) {
  # Check that required columns exist in cells and pixels
  if (!all(c("x", "y") %in% colnames(cells))) {
    stop("The 'cells' data.frame must have 'x' and 'y' columns.")
  }
  if (!all(c("x", "y", "value") %in% colnames(pixels))) {
    stop("The 'pixels' data.frame must have 'x', 'y', and 'value' columns.")
  }

  # Ensure the np package is available
  if (!requireNamespace("np", quietly = TRUE)) {
    stop("Package 'np' is required. Please install it with install.packages('np').")
  }

  # If no bandwidth is provided, automatically select one using npregbw.
  if (is.null(bw)) {
    bw <- np::npregbw(formula = value ~ x + y, data = pixels, regtype = "lc", ckertype = "gaussian")
  }

  # Fit the kernel regression model using the selected or provided bandwidth.
  fit <- np::npreg(bws = bw)

  # Predict the surface values (z) at the cell locations.
  predictions <- predict(fit, newdata = cells)

  return(predictions)
}
