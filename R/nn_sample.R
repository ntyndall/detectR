#' @title Neural Network Data Sample
#' @export


nn_sample <- function(trainingData, posClass = "N", normalData = 2000, percent = 80, logs) {

  # Report on function
  maliciousData <- normalData %>% `*`(1 %>% `-`(percent %>% `/`(100)))
  if (logs) {
    cat(
      crayon::cyan(
        paste0(
          "\n ## 1) Sample data set of size : ", trainingData %>% nrow, "\n       to contain `",
          normalData, "` normal events and `", maliciousData, "` malicious. \n"
        )
      )
    )
  }

  # Check what labels are available, and how many
  uniqLabels <- trainingData$label %>% unique
  uLen <- uniqLabels %>% length

  # Make sure requisites are met.
  if (posClass %in% uniqLabels %>% `!`()) stop(crayon::red(" ## Training data must contain NORMAL traffic."))
  if (trainingData$label %>% `==`(posClass) %>% sum %>% `<`(normalData)) {
    stop(crayon::red(" ## Increase data for positive class, or decrease `normalData` input."))
  }
  if (uLen %>% `==`(1)) stop(crayon::red(" ## Select at least 2 unique type classes."))
  if (trainingData$label %>% `!=`(posClass) %>% sum %>% `<`(maliciousData)) {
    stop(crayon::red(" ## Increase data for negative classes, or decrease `normalData` input."))
  }
  if (trainingData$type %>% unique %>% length %>% `==`(1)) trainingData$type <- NULL

  # Make sure normal data is ordered and subsetted first
  # e.g. 'S', 'N', 'X' --> 'N', 'S', 'X'
  uniqLabels <- c(posClass, uniqLabels %>% subset(uniqLabels != posClass))

  # Create a combined data set on types
  allData <- data.frame(stringsAsFactors = FALSE)

  for (i in 1:uLen) {
    labData <- trainingData %>% subset(trainingData$label == uniqLabels[i])

    # Base results of normal data (always in position one)
    if (i %>% `==`(1)) {
      pivotRow <- numRow <- normalData %>%
        min(labData %>% nrow)
    } else {
      numRow <- `*`(pivotRow, `/`(`/`(100 - percent, 100), uLen - 1)) %>%
        round
    }

    # Sample from the complete data set included for a particular label
    allData %<>% rbind(labData[labData %>% nrow %>% sample %>% head(numRow), ])
  }

  # Return sampled data
  return(allData)
}
