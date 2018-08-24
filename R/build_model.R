#' @title Builder
#'
#' @param trainingData A data frame that contains attack strings, types, and
#'  labels, followed by columns with any number of anomaly features for building
#'  the model - must have at least 2 features to run.
#' @param normalData An integer
#'
#' @export


builder <- function(d.set, normalData = 2000, percent = 80) {

  # Sample the data set first
  d.set %<>% detectR::nn_sample(
    normalData = normalData,
    percent = percent
  )

  # Generate scales based on d.set
  dataScales <- d.set %>%
    detectR::nn_gen_scales()

  # Actually scale any data required (then split it up)
  d.set %<>% detectR::nn_scaler(
    dataScales = dataScales
  )

  # Split the data set up into training / testing
  train.test <- d.set %>%
    detectR::nn_split()

  # Build a neural network
  nn <- train.test$train %>%
    detectR::nn_build(
      dataScales = dataScales
    )

  # Test predictions
  testResults <- train.test$test %>%
    detectR::nn_test(
      dataScales = dataScales,
      nn = nn
    )

  # Return the neural network and the data scales
  return(
    list(
      nn = nn,
      dataScales = dataScales
    )
  )
}
