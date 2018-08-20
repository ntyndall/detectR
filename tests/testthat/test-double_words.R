
# Initialise list
arg <- "this is or not a test"
testMatch <- detectR::parse_words(
  tokens = arg %>% detectR::tokenization(),
  args = arg
)


# Begin tests
test_that("Test that a double barrel sqli word can adjust the matching token information.", {

  charLen <- testMatch$wordList[[1]] %>% length
  result <- testMatch %>% detectR::double_words()

  expect_equal( result$wordList[[1]][3], 'ornot' )
  expect_equal( result$indexes[[1]], testMatch$indexes %>% `[[`(1) %>% `[`(1:(charLen - 1)) )

})

test_that("Double barrel word is caught at the start", {

  testMatch$wordList[[1]] <- c('or', 'not', 'this', 'is', 'a', 'test')
  charLen <- testMatch$wordList[[1]] %>% length
  result <- testMatch %>% detectR::double_words()

  expect_equal( result$wordList[[1]][1], 'ornot' )
  expect_equal( result$indexes[[1]], testMatch$indexes %>% `[[`(1) %>% `[`(1:(charLen - 1)) )

})

test_that("Double barrel word is caught at the end", {

  testMatch$wordList[[1]] <- c('this', 'is', 'a', 'test', 'or', 'not')
  charLen <- testMatch$wordList[[1]] %>% length
  result <- testMatch %>% detectR::double_words()

  expect_equal( result$wordList[[1]][5], 'ornot' )
  expect_equal( result$indexes[[1]], testMatch$indexes %>% `[[`(1) %>% `[`(1:(charLen - 1)) )

})
