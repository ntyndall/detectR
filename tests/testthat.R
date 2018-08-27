library(testthat)
library(covr)
library(hashmap)
library(purrr)
library(magrittr)

# Run the tests
print("beginning the tests")
results <- testthat::test_dir(
  path = "testthat",
  reporter = "summary"
)
print("tests are finished")
