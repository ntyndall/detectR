

# Begin tests
test_that("Test that a neural network can be build", {

  result <- detectR::builder(
    normalData = 1000,
    percent = 80,
    detectR::d.normal[1:1000, ],
    detectR::d.sqli[1:205, ],
    detectR::d.xss[1:205, ],
    detectR::d.bash[1:205, ]
  )

  expect_is( result, "list" )
  expect_equal( result %>% length, 2 )
  expect_equal( result %>% names, c("nn", "dataScales") )
  expect_equal( result$dataScales$labels, c("N", "S", "X", "B") )

})
