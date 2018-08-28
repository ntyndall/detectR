

# Begin tests
test_that("Check that each classification type can be achieved", {

  # Test each individual class
  expect_equal( "this is a test" %>% detectR::detectR(), "N" )
  expect_equal( "more ../../../pwd" %>% detectR::detectR(), "B" )
  expect_equal( "<script>alert('1')</script>" %>% detectR::detectR(), "X" )
  expect_equal( "SELECT * FROM Users WHERE UserId = 105 OR 1=1;" %>% detectR::detectR(), "S" )

  # Can multiple arguments be classified?
  expect_equal( c("this is a test", "more ../../../pwd") %>% detectR::detectR(), c("N", "B") )

  # As a list?
  expect_equal( list("this is a test", "more ../../../pwd") %>% detectR::detectR(), c("N", "B") )

})

