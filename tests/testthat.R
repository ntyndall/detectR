library(testthat)
library(covr)
library(hashmap)
library(purrr)
library(magrittr)

# Run the tests
results <- testthat::test_dir(
  path = "testthat",
  reporter = "summary"
)
