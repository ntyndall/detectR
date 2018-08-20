#' @title Parse Words
#'
#' @export


parse_words <- function(tokens, args) {

  # List of words
  words <- args %>%
    strsplit(split = '[[:digit:][:punct:][:blank:]]') %>%
    lapply(detectR::subset_empty)

  # List of word locations
  ind <- tokens %>%
    strsplit(split = '') %>%
    lapply(detectR::word_in_vec)

  # List of tokens
  tlist <- tokens %>%
    strsplit(split = '')


  # Return list back of information
  return(
    lapply(
      X = 1:(args %>% length),
      FUN = function(x) {
        list(
          wordList = words[[x]],
          indexes = ind[[x]],
          tokenList = tlist[[x]]
        )
      }
    )
  )
}

