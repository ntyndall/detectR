#' @title Neural Network Creator
#'
#' @param trainingData A data frame that contains attack strings, types, and
#'  labels, followed by columns with any number of anomaly features for building
#'  the model - must have at least 2 features to run.
#' @param normalData An integer
#'
#' @export


nn_creator <- function(d.features, posClass = "N", normalData = 2000, percent = 80, logs) {

  # Sample the data set first
  d.features %<>% detectR::nn_sample(
    posClass = posClass,
    normalData = normalData,
    percent = percent,
    logs = logs
  )

  # Generate scales based on d.set
  dataScales <- d.features %>%
    detectR::nn_gen_scales(
      logs = logs
    )

  # Actually scale any data required (then split it up)
  d.features %<>% detectR::nn_scaler(
    dataScales = dataScales,
    logs = logs
  )

  # Split the data set up into training / testing
  train.test <- d.features %>%
    detectR::nn_split(
      logs = logs
    )

  # Build a neural network
  nn <- train.test$train %>%
    detectR::nn_build(
      dataScales = dataScales,
      logs = logs
    )

  # Test predictions
  testResults <- train.test$test %>%
    detectR::nn_test(
      dataScales = dataScales,
      nn = nn,
      logs = logs
    )

  # Return the neural network and the data scales
  return(
    list(
      nn = nn,
      dataScales = dataScales
    )
  )
}
