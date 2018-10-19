#' @title detectR
#'
#' @export
#'
#' @useDynLib detectR maxinter distval
#'
#' @importFrom magrittr %<>% %>%


detectR <- function(x, ...) {

  # Capture any additional input
  modelInfo <- list(...)

  # Figure out the correct model to use
  if (modelInfo %>% length %>% `>`(0)) {
    if ("new.model" %in% modelInfo) {
      model.nn <- modelInfo$new.model$nn
      model.sclaes <- modelInfo$new.model$dataScales
    } else {
      stop("Provide neural network models as `new.model`")
    }
  } else {
    # Just use the packages own if none are provided
    model.nn <- detectR::nn
    model.scales <- detectR::dataScales
  }

  # Read the input
  if (x %>% typeof %>% `==`("list")) x %<>% purrr::flatten_chr()

  # Calculate features and scale
  scaled.set <- x %>%
    detectR::features() %>%
    detectR::nn_scale(
      scaler = model.scales
    )

  # Compute Predictions off Test Set
  predictions <- neuralnet::compute(
    x = model.nn,
    covariate = scaled.set
  )

  # Find the predicted integer and map to label for every row
  return(
    model.nn$model.list$response %>%
    `[`(predictions$net.result %>% apply(1, which.max))
  )
}
