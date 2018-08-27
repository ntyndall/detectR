#' @title Neural Network Calculate Scales
#' @export


nn_gen_scales <- function(allData, logs, infCols = 1) {

  # Report on function
  if (logs) {
    cat(crayon::cyan(" \n ## 2) Creating scales for manipulating training and testing datasets. \n"))
  }
  # Convert to integers (3 is the standard non-feature part of the data set)
  totCols <- ncol(allData) %>% `-`(1)
  features <- allData[ , infCols:totCols] %>% names
  allData[features] %<>% lapply(as.integer)

  # Create Vector of Column Max and Min Values
  maxs <- apply(allData[ , infCols:totCols], 2, max)
  mins <- apply(allData[ , infCols:totCols], 2, min)

  # Return list as a scaler object
  return(
    list(
      maxScale = maxs,
      minScale = mins,
      features = features,
      labels = allData$label %>% unique
    )
  )
}
