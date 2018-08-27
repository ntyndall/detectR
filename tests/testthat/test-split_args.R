

# Begin tests
test_that("Split a single query argument", {

  args <- "/?test=arg"
  result <- args %>% detectR::split_args()

  expect_is( result, "data.frame" )
  expect_equal( result %>% nrow, 1 )
  expect_equal( result$argNames, "test" )
  expect_equal( result$args, "arg" )

})

test_that("Split two arguments", {

  args <- "/?test1=arg1&test2=arg2"
  result <- args %>% detectR::split_args()

  expect_is( result, "data.frame" )
  expect_equal( result %>% nrow, 2 )
  expect_equal( result$argNames, c("test1", "test2") )
  expect_equal( result$args, c("arg1", "arg2") )

})

test_that("Combine multiple character strings and split them", {

  args <- c("/?test1=arg1&test2=arg2", "/?test3=arg3")
  result <- args %>% detectR::split_args()

  expect_is( result, "data.frame" )
  expect_equal( result %>% nrow, 3 )
  expect_equal( result$argNames, c("test1", "test2", "test3") )
  expect_equal( result$args, c("arg1", "arg2", "arg3") )

})

test_that("Replace illegal characters such as euro sign", {

  args <- "/?test=arg€*"
  result <- args %>% detectR::split_args()

  expect_is( result, "data.frame" )
  expect_equal( result %>% nrow, 1 )
  expect_equal( result$argNames, "test" )
  expect_equal( result$args, "arg*" )

})
