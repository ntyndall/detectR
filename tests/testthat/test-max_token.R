

# Begin tests
test_that("Send in a small token", {

  myStr <- "w"
  results <- myStr %>%
    detectR::max_token()

  expect_is( results, "list" )
  expect_equal( results$token, myStr %>% substr(start = 1, stop = 5) )
  expect_equal( results$score, 2 )

})

test_that("Send in a token greater than 5 length", {

  myStr <- "wswswswsws"
  results <- myStr %>%
    detectR::max_token()

  expect_is( results, "list" )
  expect_equal( results$token, myStr %>% substr(start = 1, stop = 5) )
  expect_equal( results$score, 10 )

})

test_that("Detect various attack tokens", {

  for (x in c("x", "y", "z")) {
    myStr <- paste0("ws", x, "sw")
    results <- myStr %>%
      detectR::max_token()

    expect_is( results, "list" )
    expect_equal( results$token, myStr %>% substr(start = 1, stop = 5) )
    expect_equal( results$score, 38 )
  }

})


test_that("Detect attack tokens at various parts of a string", {

  base <- c("wswswswswswsws")

  for (x in 1:3) {
    myStr <- if (x == 1) {
      paste0("x", base)
    } else if (x == 2) {
      paste0(base, "x")
    } else {
      paste0(base, "x", base)
    }

    results <- myStr %>%
      detectR::max_token()

    expect_is( results, "list" )
    expect_equal( results$token %>% nchar, 5 )
    expect_equal( results$score, 38 )
  }

})

