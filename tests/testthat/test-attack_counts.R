
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
  results <- matching %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( results$counts, "matrix" )
  expect_equal( results$counts %>% nrow, 1 )
  expect_equal( results$counts %>% ncol, 3 )
  expect_equal( results$counts %>% as.double, 0 %>% rep(3) )
  expect_equal( results$matched[[1]]$tokenList, matching[[1]]$tokenList )

  # Can multiple items be passed?
  results <- c(matching, matching) %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( results$counts, "matrix" )
  expect_equal( results$counts %>% nrow, 2 )
  expect_equal( results$counts %>% ncol, 3 )
  expect_equal( results$counts %>% as.double, 0 %>% rep(6) )
  for (i in 1:2) expect_equal( results$matched[[1]]$tokenList, matching[[1]]$tokenList )

})

test_that("Can attacks such as sqli/xss/bash be detected?", {

  # Set up vector and paste attack token either end
  basevec <- c("s", "w", "s", "w", "s")

  # Check for sqli
  matching[[1]]$wordList <- c('ornot', 'is', 'a', 'select')
  results <- matching %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( results$counts, "matrix" )
  expect_equal( results$counts %>% nrow, 1 )
  expect_equal( results$counts %>% ncol, 3 )
  expect_equal( results$counts[1, 1], 2 )
  expect_equal( results$matched[[1]]$tokenList, c("x", basevec, "x") )

  # Check for xss
  matching[[1]]$wordList <- c('alert', 'is', 'a', 'alert')
  results <- matching %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( results$counts, "matrix" )
  expect_equal( results$counts %>% nrow, 1 )
  expect_equal( results$counts %>% ncol, 3 )
  expect_equal( results$counts[1, 2], 2 )
  expect_equal( results$matched[[1]]$tokenList, c("y", basevec, "y") )

  # Check for xss
  matching[[1]]$wordList <- c('pwd', 'is', 'a', 'pwd')
  results <- matching %>%
    detectR::attack_counts(
      attackWords = attackWords
    )

  expect_is( results$counts, "matrix" )
  expect_equal( results$counts %>% nrow, 1 )
  expect_equal( results$counts %>% ncol, 3 )
  expect_equal( results$counts[1, 3], 2 )
  expect_equal( results$matched[[1]]$tokenList, c("z", basevec, "z") )

})
