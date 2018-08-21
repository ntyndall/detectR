
# Initialise the attack words
attackWords <- detectR::attack_keywords()

# Define a simple matching object
matching <- list(
  wordList = c("this", "is", "a", "test"),
  indexes = c(1, 3, 5, 7),
  tokenList = c("w", "s", "w", "s", "w", "s", "w")
) %>%
  list


# Begin tests
test_that("Make sure a normal string returns 0 for all attack counts", {

  # There should be no attack words
  counts <- matching %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( counts, "matrix" )
  expect_equal( counts %>% nrow, 1 )
  expect_equal( counts %>% ncol, 3 )
  expect_equal( counts %>% as.double, 0 %>% rep(3) )

  # Can multiple items be passed?
  counts <- c(matching, matching) %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( counts, "matrix" )
  expect_equal( counts %>% nrow, 2 )
  expect_equal( counts %>% ncol, 3 )
  expect_equal( counts %>% as.double, 0 %>% rep(6) )

})

test_that("Can attacks such as sqli/xss/bash be detected?", {

  # Check for sqli
  matching[[1]]$wordList <- c('ornot', 'is', 'a', 'select')
  counts <- matching %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( counts, "matrix" )
  expect_equal( counts %>% nrow, 1 )
  expect_equal( counts %>% ncol, 3 )
  expect_equal( counts[1, 1], 2 )

  # Check for xss
  matching[[1]]$wordList <- c('alert', 'is', 'a', 'alert')
  counts <- matching %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( counts, "matrix" )
  expect_equal( counts %>% nrow, 1 )
  expect_equal( counts %>% ncol, 3 )
  expect_equal( counts[1, 2], 2 )

  # Check for xss
  matching[[1]]$wordList <- c('pwd', 'is', 'a', 'pwd')
  counts <- matching %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( counts, "matrix" )
  expect_equal( counts %>% nrow, 1 )
  expect_equal( counts %>% ncol, 3 )
  expect_equal( counts[1, 3], 2 )

})
