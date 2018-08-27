#' @title Neural Network Predictions
#' @export


nn_test <- function(testData, dataScales, nn, logs, arguments = NULL) {

  # Report on function
  if (logs) cat(crayon::cyan(" \n ## 6) Creating predictions and building confusion matrix ## \n"))

  # Initialise empty dataframe
  testResults <- data.frame(stringsAsFactors = FALSE)

  # Compute Predictions off Test Set
  predictions <- neuralnet::compute(
    x = nn,
    covariate = testData[ , dataScales$features]
  )

  # Create vectors to measure accuracy
  realVec <- predVec <- '' %>% rep(testData %>% nrow)
  for (i in 1:(dataScales$labels %>% length)) {
    cLab <- dataScales$labels[i]
    realVec[testData[[cLab]] %>% `==`(1) %>% which] <- cLab
  }

  # Check the max values per row for the predictions
  netRes <- predictions$net.result
  for (j in 1:(netRes %>% nrow)) {
    predVec[j] <- dataScales$labels[netRes[j, ] %>% which.max]

    # Return the argument strings if further analysis on FP / FN is required
    if (arguments %>% is.null %>% `!`()) {
      if (predVec[j] %>% `!=`(realVec[j])) {
        testResults %<>% rbind(
          data.frame(
            jval = j,
            argument = arguments[j],
            actual = realVec[j],
            predicted = predVec[j],
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  # Convert to factors
  OTHER <- realVec %>% factor(levels = dataScales$labels)
  NN <- predVec %>% factor(levels = dataScales$labels)

  # Build a table of results
  myT <- table(OTHER, NN)

  # Print the confusion matrix of results
  confMat <- caret::confusionMatrix(data = myT)
  if (logs) print(confMat)

  # Return the FP / FN results as a data frame
  return(
    list(
      offdiag = testResults,
      acc = confMat$overall[['Accuracy']]
    )
  )
}
