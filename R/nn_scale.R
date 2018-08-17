#' @title Neural Network Scaling
#'
#' @export


nn_scale <- function(feat.set) {
  # Lift the scaling list from the package
  myScaling <- detectR::dataScales

  # Scale all features
  scaled.data <- feat.set %>%
    scale(
      center = myScaling$minScale,
      scale = myScaling$maxScale %>% `-`(myScaling$minScale)
    ) %>%
    as.data.frame()

  # Rename column names as each feature
  names(scaled.data) <- myScaling$features

  return(scaled.data)
}
