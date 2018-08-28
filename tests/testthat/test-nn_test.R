

# Begin tests
test_that("Return off diagonal results as a data frame", {

  # Get arguments
  args <- detectR::d.normal$argument[1:5]

  # Get features...
  my.feats <- detectR::d.normal$argument[1:5] %>% detectR::features()

  # ...and scale them
  new <- my.feats %>% detectR::nn_scale()

  # Compute results from nn testing
  results <- new %>%
    detectR::nn_test(
      dataScales = detectR::dataScales,
      nn = detectR::nn,
      logs = TRUE,
      arguments = args
    )

  expect_is( results$offdiag, "data.frame" )
  expect_equal( results$offdiag %>% nrow, 5 )

})
