

# Begin tests
test_that("Test that a neural network can be built", {

  result <- detectR::builder(
    normalData = 200,
    percent = 80,
    logs = TRUE,
    detectR::d.normal[1:200, ],
    detectR::d.sqli[1:100, ],
    detectR::d.xss[1:100, ],
    detectR::d.bash[1:100, ]
  )

  expect_is( result, "list" )
  expect_equal( result %>% length, 2 )
  expect_equal( result %>% names, c("nn", "dataScales") )

})
