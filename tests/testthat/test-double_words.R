testMatch <- list(
  wordList = c('this', 'is', 'or', 'not', 'a', 'test') %>% list,
  indexes = c(2, 4, 6, 8, 10, 12) %>% list,
  tokenList =  c('s', 'w', 's', 'w', 's', 'w', 's', 'w', 's', 'w', 's', 'w') %>% list
)

# Begin tests
test_that("Test that a double barrel sqli word can adjust the matching token information.", {

  charLen <- testMatch$wordList[[1]] %>% length
  result <- testMatch %>% detectR::double_words()

  expect_equal( result$wordList[[1]][3], 'ornot' )
  expect_equal( result$indexes[[1]], testMatch$indexes %>% `[[`(1) %>% `[`(1:(charLen - 1)) )

})
