

# Begin tests
test_that("Can a feature set be created", {

  # Single token
  args <- "this is a test"
  results <- args %>%
    detectR::features()

  expect_is( results, "data.frame" )
  expect_equal( results %>% nrow, 1 )

})
