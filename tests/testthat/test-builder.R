

# Begin tests
test_that("Test that a neural network can be build", {

  print("about to build?!")
  result <- detectR::builder(
    normalData = 100,
    percent = 80,
    logs = T,
    detectR::d.normal[1:100, ],
    detectR::d.sqli[1:50, ],
    detectR::d.xss[1:50, ],
    detectR::d.bash[1:50, ]
  )

  print("and the scales?!")
  print(result$dataScales)
  expect_is( result, "list" )
  expect_equal( result %>% length, 2 )
  expect_equal( result %>% names, c("nn", "dataScales") )
  expect_equal( result$dataScales$labels, c("N", "S", "X", "B") )

})
