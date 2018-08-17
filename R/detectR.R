#' @title detectR
#'
#' @export
#'
#' @useDynLib detectR maxinter distval


detectR <- function(x) {

  # Read the input
  if (x %>% typeof %>% `==`("list")) x %<>% purrr::flatten_chr()

  # Calculate features and scale
  scaled.set <- x %>%
    detectR::features() %>%
    detectR::nn_scale()

  # Compute Predictions off Test Set
  predictions <- neuralnet::compute(
    x = detectR::nn,
    covariate = scaled.set
  )

  # Find the predicted integer and map to label for every row
  return(
    detectR::dataScales$labels %>%
    `[`(predictions$net.result %>% apply(1, which.max))
  )
}
