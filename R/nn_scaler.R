#' @title Neural Network Scale Data
#' @export


nn_scaler <- function(allData, dataScales, logs) {

  # Report on function
  if (logs) {
    cat(
      crayon::cyan(
        paste0(
          " \n ## 3) Scaling the data set with : \n",
          "       Features : ", dataScales$features %>% length, "\n",
          "       Labels : ", dataScales$labels %>% length, "\n"
        )
      )
    )
  }

  # Use scale() and convert the resulting matrix to a data frame
  scaled.data <- allData[ , dataScales$features] %>%
    scale(
      center = dataScales$minScale,
      scale =  dataScales$maxScale %>% `-`(dataScales$minScale)
    ) %>%
    as.data.frame

  # Set up zerod matrix and allocate the matched labels
  dSet <- matrix(
    data = 0,
    ncol = dataScales$labels %>% length,
    nrow = allData %>% nrow
  ) %>%
    as.data.frame
  names(dSet) <- dataScales$labels

  for (i in 1:(dSet %>% ncol)) {
    repl <- allData$label %>% `==`(dataScales$labels[i])
    if (repl %>% any) dSet[repl %>% which, i] <- 1
  }

  # Bind the classes to the scaled data and return
  dSet %<>% cbind(scaled.data) %>% return()
}
