

# Begin tests
test_that("Return a matrix of scores based on vector of tokens supplied", {

  # Single token
  tokens <- "wswswswsws"
  results <- tokens %>%
    detectR::max_from_list()

  expect_is( results, "matrix" )
  expect_equal( results %>% nrow, 1 )
  expect_equal( results %>% ncol, 1 )
  expect_equal( results[1, 1], 10 )

  # Multiple tokens
  tokens <- c("w", "ws")
  results <- tokens %>%
    detectR::max_from_list()

  expect_is( results, "matrix" )
  expect_equal( results %>% nrow, 2 )
  expect_equal( results %>% ncol, 1 )
  expect_equal( results[1, 1], 2 )
  expect_equal( results[2, 1], 4 )

})
