library(testthat)
library(covr)
library(hashmap)
library(purrr)
library(magrittr)
library(organisR)

# Run the tests
results <- testthat::test_dir(
  path = "testthat",
  reporter = "summary"
)

# Return response code
quit(
  save = 'no',
  status = results %>% organisR::test_output(),
  runLast = FALSE
)
