#' @title Parse Words
#'
#' @export


parse_words <- function(tokens, args) {
  return(
    list(
      wordList = args %>% strsplit(split = '[[:digit:][:punct:][:blank:]]') %>%
        lapply(detectR::subset_empty),
      indexes = tokens %>% strsplit(split = '') %>%
        lapply(detectR::word_in_vec),
      tokenList = tokens %>% strsplit(split = '')
    )
  )
}
