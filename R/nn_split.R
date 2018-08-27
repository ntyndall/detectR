#' @title Neural Network Split Data
#' @export


nn_split <- function(allData, logs, split = 70) {

  # Report on function
  if (logs) {
    cat(
      crayon::cyan(
        paste0(
          " \n ## 4) Splitting the data set : ",
          split, " / ", 100 %>% `-`(split), ".\n"
        )
      )
    )
  }

  # Create Split (any column is fine)
  split.data <- allData$N %>%
    caTools::sample.split(
      SplitRatio = split %>% `/`(100)
    )

  # Split based off of split Boolean Vector and return
  return(
    list(
      train = allData %>% subset(split.data %>% `==`(TRUE)),
      test = allData %>% subset(split.data %>% `==`(FALSE))
    )
  )
}
