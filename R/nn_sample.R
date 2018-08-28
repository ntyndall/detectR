#' @title Neural Network Data Sample
#' @export


nn_sample <- function(trainingData, posClass = "N", normalData = 2000, percent = 80, logs) {

  # Calculate count of malicious data to include
  maliciousData <- 1 %>%
    `-`(percent %>% `/`(100)) %>%
    `*`(normalData)

  # Report on function
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

  # Get number for each of the other classes
  maliciousData %<>%
    `/`(uniqLabels %>% length %>% `-`(1)) %>%
    round

  # More clearer way of subsetting data
  all.data <- lapply(
    X = uniqLabels,
    FUN = function(x) {
      # Subset the data
      sub.data <- trainingData %>% subset(trainingData$label == x)

      # Make sure enough data exists
      rowsToSub <- sub.data %>%
        nrow %>%
        min(if (x %>% `==`(posClass)) normalData else maliciousData)

      # Take a sample of the full data set
      return(
        sub.data %>%
          dplyr::sample_n(
            size = rowsToSub,
            replace = FALSE
          )
      )
    }
  ) %>%
    purrr::reduce(rbind)

  # Return sampled data
  return(all.data)
}
