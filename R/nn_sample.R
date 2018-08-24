#' @title Neural Network Data Sample
#' @export


nn_sample <- function(trainingData, normalData = 2000, percent = 80) {

  # Report on function
  maliciousData <- normalData %>% `*`(1 %>% `-`(percent %>% `/`(100)))
  cat('\n ## Sample data set of size :', trainingData %>% nrow, '\n',
      '## to contain `', normalData, '` normal events and `', maliciousData, '` malicious. \n')

  # Check what labels are available, and how many
  uniqLabels <- trainingData$label %>% unique
  uLen <- uniqLabels %>% length

  # Make sure requisites are met.
  if (uLen %>% `==`(1)) stop(' ## Select at least 2 unique type classes.')
  if ('N' %in% uniqLabels %>% `!`()) stop( ' ## Training data must contain NORMAL traffic.')
  if (trainingData$type %>% unique %>% length %>% `==`(1)) trainingData$type <- NULL

  # Make sure normal data is ordered and subsetted first
  # e.g. 'S', 'N', 'X' --> 'N', 'S', 'X'
  uniqLabels <- c('N', uniqLabels %>% subset(uniqLabels != 'N'))

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
