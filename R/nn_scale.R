#' @title Neural Network Scaling
#'
#' @export


nn_scale <- function(feat.set, scaler) {
  # Scale all features
  scaled.data <- feat.set %>%
    scale(
      center = scaler$sMin,
      scale = scaler$sMax %>% `-`(scaler$sMin)
    ) %>%
    as.data.frame()

  # Rename column names as each feature
  names(scaled.data) <- scaler$sMax %>% names

  return(scaled.data)
}
