
# Initialise list
arg <- "this is or not a test"
testMatch <- detectR::parse_words(
  tokens = arg %>% detectR::tokenization(),
  args = arg
 ) %>%
  `[[`(1)

# Initialise double list
dbls <- detectR::doubles$doublewords %>%
  as.character %>%
  strsplit(split = " ")

# Begin tests
test_that("Test that a double barrel sqli word can adjust the matching token information.", {

  charLen <- testMatch$wordList %>% length
  result <- testMatch %>%
    detectR::double_words(
      dbls = dbls
    )

  expect_equal( result$wordList[3], 'ornot' )
  expect_equal( result$indexes, testMatch$indexes %>% `[`(1:(charLen - 1)) )

})

test_that("Double barrel word is caught at the start", {

  testMatch$wordList <- c('or', 'not', 'this', 'is', 'a', 'test')
  charLen <- testMatch$wordList %>% length
  result <- testMatch %>%
    detectR::double_words(
      dbls = dbls
    )

  expect_equal( result$wordList[1], 'ornot' )
  expect_equal( result$indexes, testMatch$indexes %>% `[`(1:(charLen - 1)) )

})

test_that("Double barrel word is caught at the end", {

  testMatch$wordList <- c('this', 'is', 'a', 'test', 'or', 'not')
  charLen <- testMatch$wordList %>% length
  result <- testMatch %>%
    detectR::double_words(
      dbls = dbls
    )

  expect_equal( result$wordList[5], 'ornot' )
  expect_equal( result$indexes, testMatch$indexes %>% `[`(1:(charLen - 1)) )

})

test_that("Make sure two double barrel words can be detected", {

  testMatch$wordList <- c('wait', 'for', 'a', 'test', 'or', 'not')
  charLen <- testMatch$wordList %>% length
  result <- testMatch %>%
    detectR::double_words(
      dbls = dbls
    )

  expect_equal( result$wordList[1], 'waitfor' )
  expect_equal( result$wordList[4], 'ornot' )
  expect_equal( result$indexes, testMatch$indexes %>% `[`(1:(charLen - 2)) )

})

