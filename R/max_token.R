#' @title Max token Score
#'
#' @description The following patterns map to the tokens
#'
#' @param token A character string representing the tokenization
#'  of a string.
#'
#' @export


max_token <- function(token) {
  token %<>%
    strsplit(split = '') %>%
    purrr::flatten_chr()

  # Find out what it could be...
  maliciousTokens <- c('x', 'y', 'z')
  get_counts <- function(x) x %>% `==`(token) %>% sum
  myCounts <- maliciousTokens %>% sapply(get_counts) %>% as.integer
  elevate <- if (myCounts %>% sum %>% `==`(0)) 'N' else maliciousTokens[myCounts %>% which.max] %>% toupper

  # Get the importance hashmap
  importance <- elevate %>% detectR::token_hash()
  allowedTokens <- importance$keys()
  tokenLen <- token %>% length

  # If token is length 5 or shorter, then return
  if (`<`(tokenLen, 6)) {
    return(
      list(
        token = token %>% paste(collapse = ''),
        score = token %>% importance$find() %>% sum
      )
    )
  }

  mSum <- 0
  notAllowed <- token %in% allowedTokens
  if (notAllowed %>% all %>% `!`()) {
    token %<>% `[`(notAllowed)
    tokenLen <- token %>% length
  }

  # Get the max subset of token scores within a long string
  scores <- token %>% importance$find()
  ind <- .Fortran(
    detectR:::maxinter,
    vec = as.double(scores),
    vLen = as.integer(length(scores)),
    currentInd = as.integer(0)
  )$currentInd
  ranges <- ind:(ind + 4)

  # Return both max token and score
  return(
    list(
      token = token[ranges] %>% paste(collapse = ''),
      score = sum(scores[ranges])
    )
  )
}
