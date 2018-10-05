#' @title Tokenization attributes
#' @export


features <- function(args) {

  # Tokenize stuff
  args %<>% tolower
  tokens <- args %>%
    detectR::tokenization()

  # Create character distribution
  chardist <- args %>%
    detectR::char_dist()

  # Split by spaces / collapse string / create a matrix of column 1
  scores <- tokens %>%
    detectR::max_from_list()

  # Create a list object to do matching on
  matching <- detectR::parse_words(
    tokens = tokens,
    args = args
  )

  # Define double word combinations
  dbls <- detectR::doubles$doublewords %>%
    as.character %>%
    strsplit(split = " ")

  # Check for doubled words like `make set` etc..
  matching %<>% lapply(
    FUN = function(x) x %>% detectR::double_words(dbls = dbls)
  )

  # Replace and count all attack words with specialised tokens
  results <- matching %>%
    detectR::attack_counts(
      attackWords = detectR::attack_keywords()
    )

  # Shouldn't need to collapse them (but keep for now)
  escores <- results$matched %>%
    purrr::map("tokenList") %>%
    lapply(function(x) x %>% paste(collapse = '')) %>%
    detectR::max_from_list()

  # Column bind all the results together
  feat.set <- cbind(
    scores,
    escores,
    chardist,
    results$counts,
    args %>% detectR::special_sql(),
    args %>% detectR::special_xss(),
    args %>% detectR::special_bash()
  ) %>%
    as.data.frame

  # Rename with the feature names
  names(feat.set) <- detectR::feat_names()

  # And return data frame back
  return(feat.set)
}
