#' @title Neural Network Creator
#'
#' @param trainingData A data frame that contains attack strings, types, and
#'  labels, followed by columns with any number of anomaly features for building
#'  the model - must have at least 2 features to run.
#' @param normalData An integer
#'
#' @export


nn_creator <- function(d.features, logs, nnThresh,
                       posClass = "N", normalData = 10000, percent = 80) {

  # Sample the data set first
  d.features %<>% detectR::nn_sample(
    posClass = posClass,
    normalData = normalData,
    percent = percent,
    logs = logs
  )

  # Scale and transform data
  cat(crayon::cyan(" ## 2) Scaling data \n"))
  scaled.info <- d.features %>%
    mltools::scale_data(
      cLabel = "label"
    )

  # Build Neural network
  cat(crayon::cyan(" ## 3) Building neural network \n"))
  results <- scaled.info$data %>%
    mltools::gen_nn(
      logs = logs,
      NN = list(THRESH = nnThresh)
    )

  # Print out Confusion Matrix
  cat(crayon::cyan(" ## 4) Reporting on results \n"))
  accuracy <- results$totalStats$totAcc
  cat(crayon::cyan("    ## Average accuracy:", accuracy %>% mean, "+/-", accuracy %>% stats::sd(), "\n"))
  print(myres$CM)

  # Return the neural network and the data scales
  return(
    list(
      nn = results$model,
      dataScales = scaled.info$scaler
    )
  )
}
