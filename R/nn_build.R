#' @title Neural Network Builder
#' @export


nn_build <- function(training, dataScales, logs) {

  # Report on function
  if (logs) cat(crayon::cyan("\n ## 5) Building neural network ## \n"))

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
  if (logs) cat(" ## ")
  nn <- neuralnet::neuralnet(
    formula = f,
    data = training,
    hidden = neurons %>% rep(2),
    act.fct = "logistic",
    linear.output = FALSE,
    lifesign = if (logs) 'minimal' else "none",
    stepmax = 1000000
  )

  # Return the neural network
  return(nn)
}
