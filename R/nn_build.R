#' @title Neural Network Builder
#' @export


nn_build <- function(training, dataScales) {

  # Report on function
  cat('\n ## Building neural network ## \n')

  f <- paste0(
    dataScales$labels %>% paste(collapse = " + "),
    " ~",
    paste(dataScales$features, collapse = " + ")
  ) %>%
    as.formula

  # Calculate number of neurons
  neurons <- dataScales$features %>%
    length %>%
    `+`(dataScales$labels %>% length) %>%
    `/`(2) %>%
    round %>%
    `+`(1)

  # Build the neural network
  cat(" ## ")
  nn <- neuralnet::neuralnet(
    formula = f,
    data = training,
    hidden = neurons %>% rep(2),
    act.fct = "logistic",
    linear.output = FALSE,
    lifesign = 'minimal',
    stepmax = 1000000
  )

  # Return the neural network
  return(nn)
}
