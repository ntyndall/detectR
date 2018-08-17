

# Begin tests
test_that("Single character + single character vector", {

  args <- 'a'
  result <- args %>% detectR::char_dist()

  expect_is(result, 'matrix')
  expect_equal(result %>% nrow, args %>% length)
  expect_equal(result[1, ] %>% length, 6)
  expect_equal(result[1, ], c(1, 0, 0, 0, 0, 0))

})


test_that("Multiple character length + single character vector", {

  args <- 'abc'
  result <- args %>% detectR::char_dist()

  expect_is(result, 'matrix')
  expect_equal(result %>% nrow, args %>% length)
  expect_equal(result[1, ] %>% length, 6)
  expect_equal(result[1, ], c(1, 2, 0, 0, 0, 0))

})


test_that("Multiple character length + multiple character vector", {

  args <- c('abc', 'aaabbbccc')
  result <- args %>% detectR::char_dist()

  expect_is(result, 'matrix')
  expect_equal(result %>% nrow, args %>% length)
  expect_equal(result[1, ] %>% length, 6)
  expect_equal(result[1, ], c(1, 2, 0, 0, 0, 0))
  expect_equal(result[1, ] * 3, result[2, ])

})
