#' @title Builder
#'
#' @export


builder <- function(...) {
  cat(crayon::magenta(" --{ Building Neural Network Model }-- \n\n "))

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
  d.set <- rbind(
    d.normal,
    detectR::d.sqli,
    detectR::d.xss,
    detectR::d.bash
  )

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
      normalData = 2000,
      percent = 80
    )
  cat(crayon::cyan(" }-- \n"))

  # ... and save it
  cat(crayon::cyan(" --{ Saving to /data/"))
  save(results$nn, file = getwd() %>% paste0("/data/nn.rda"))
  save(results$dataScales, file = getwd() %>% paste0("/data/dataScales.rda"))
  cat(crayon::cyan(" }-- \n"))
}
