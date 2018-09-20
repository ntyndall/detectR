#' @title Builder
#'
#' @description This function provides a useful flow for building
#'  custom models based on the \code{neuralnet} package with
#'  custom data sets and input.
#'
#' @param posClass A character string, defining the positive class
#'  of a dataset, default to \code{N}.
#' @param saveData A boolean value that also saves the model and
#'  scaler, as well as returning the results.
#' @param normalData An integer value that defines the number of
#'  rows of data to sample from the positive class.
#' @param percent A double value between 0 and 100 that defines the
#'  percentage of data that the normal data should make up the full
#'  data set, the remaining percent is split equally among the other
#'  negative classes.
#' @param logs A boolean value which defines whether logs should be
#'  printed to screen, default to \code{TRUE}.
#'
#' @export


builder <- function(..., posClass = "N", saveData = FALSE,
                    normalData = 10000, percent = 80, logs = TRUE,
                    nnThresh = 0.01) {

  # Log header details first
  cat(crayon::cyan("\n\n --{ Building Neural Network Model }-- \n\n"))

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
  } else { # nocov start
    # Make sure the normal data set is built
    fName <- getwd() %>%
      paste0("/data/d.normal.rda")

    # Build it, and load it
    if (fName %>% file.exists) {
      d.normal <- detectR::d.normal
    } else {
      cat(crayon::cyan(" --{ Building normal data "))
      detectR::prepare()
      load(file = fName)
      cat(crayon::cyan("}--"))
    }

    # Load in all data sets
    rbind(
      d.normal,
      detectR::d.sqli,
      detectR::d.xss,
      detectR::d.bash
    )
  } # nocov end

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
      percent = percent,
      nnThresh = nnThresh,
      logs = logs
    )
  cat(crayon::cyan(" }-- \n"))

  # ... and save it
  if (saveData) { # nocov start
    cat(crayon::cyan(" --{ Saving to /data/"))
    save(results$nn, file = getwd() %>% paste0("/data/nn.rda"))
    save(results$dataScales, file = getwd() %>% paste0("/data/dataScales.rda"))
    cat(crayon::cyan(" }-- \n"))
  } # nocov end

  # Return the results back
  return(results)
}
