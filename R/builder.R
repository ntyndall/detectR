#' @title Builder
#'
#' @export


builder <- function(..., posClass = "N", saveData = FALSE, normalData = 2000, percent = 80) {
  # Log header details first
  cat(crayon::magenta(" --{ Building Neural Network Model }-- \n "))

  # Check input first
  input <- list(...)

  # Arrange the data set
  d.set <- if (input %>% length %>% `>`(0)) {
    # Get data frame names
    dataNames <- input %>%
      purrr::map(names)

    # Make sure they are the same length
    dLen <- dataNames %>%
      purrr::map(length) %>%
      purrr::flatten_dbl()

    if (dLen %>% `!=`(2) %>% any) {
      stop(crayon::red(" ## Ensure all data frames have only 2 columns."))
    }

    # Make sure all column names are the same, even if they are already
    for (i in input) names(i) <- c("argument", "label")
    input %>% purrr::reduce(rbind)
  } else {
    # Make sure the normal data set is built
    fName <- getwd() %>%
      paste0("/data/d.normal.rda")

    # Build it, and load it
    if (fName %>% file.exists) {
      d.normal <- detectR::d.normal
    } else {
      cat(crayon::cyan(" --{ Building normal data \n"))
      detectR::prepare()
      load(file = fName)
    }

    # Load in all data sets
    rbind(
      d.normal,
      detectR::d.sqli,
      detectR::d.xss,
      detectR::d.bash
    )
  }

  # Build the feature sets
  cat(crayon::cyan(" --{ Building feature set"))
  d.features <- d.set$argument %>%
    detectR::features()
  cat(crayon::cyan(" }-- \n"))

  # Append the labels onto the data set
  d.features$label <- d.set$label

  # Build the model ...
  cat(crayon::cyan(" --{ Building model \n"))
  results <- d.features %>%
    detectR::nn_creator(
      posClass = posClass,
      normalData = normalData,
      percent = percent
    )
  cat(crayon::cyan(" }-- \n"))

  # ... and save it
  if (saveData) {
    cat(crayon::cyan(" --{ Saving to /data/"))
    save(results$nn, file = getwd() %>% paste0("/data/nn.rda"))
    save(results$dataScales, file = getwd() %>% paste0("/data/dataScales.rda"))
    cat(crayon::cyan(" }-- \n"))
  }

  # Return the results back
  return(results)
}
