#' @title Token hash
#' @export


token_hash <- function(elevate) {
  # Malicious tokens
  malTokens <- c('x', 'y', 'z')

  # x / y / z / are reserved for sqli / xss / bash
  baseTokens <- c('w', 'n', 's', 'd', 'p', 'b', 'a', 'c', 'f', 'o', 't', 'j', 'l', 'k')
  maliciousTokens <- c(baseTokens, malTokens)

  if (elevate == 'N') {
    allowedTokens <- baseTokens
    scores <- c(2, 2, 2, 4, 4, 4, 6, 3, 7, 10, 7, 8, 10, 8)
  } else if (elevate == 'X') {
    allowedTokens <- maliciousTokens
    scores <- c(2, 2, 2, 6, 8, 4, 6, 3, 7, 10, 12, 20, 20, 8, 30, 10, 10)
  } else if (elevate == 'Y') {
    allowedTokens <- maliciousTokens
    scores <- c(2, 2, 2, 6, 8, 4, 6, 3, 7, 10, 12, 12, 15, 8, 10, 30, 10)
  } else if (elevate == 'Z') {
    allowedTokens <- maliciousTokens
    scores <- c(2, 2, 2, 15, 8, 4, 6, 3, 7, 15, 8, 8, 15, 8, 10, 10, 30)
  }

  # Return the correct hashmap
  return(
    hashmap::hashmap(
      keys = allowedTokens,
      values = scores %>% as.integer
    )
  )
}
