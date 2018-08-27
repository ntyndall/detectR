library(testthat)
library(covr)
library(hashmap)
library(purrr)
library(magrittr)

# Run the tests
cat("beginning the tests \n")
results <- testthat::test_dir(
  path = "testthat",
  reporter = "summary"
)
cat("tests are finished \n")
