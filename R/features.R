#' @title Tokenization attributes
#' @export


features <- function(args) {

  # Initialise attack keywords
  detectR::attack_keywords()
  attackNames <- attackWords %>% names

  # Tokenize stuff
  args %<>% tolower
  tokens <- args %>% detectR::tokenization()

  # Create character distribution
  chardist <- args %>% detectR::char_dist()

  # Split by spaces / collapse string / create a matrix of column 1
  scores <- tokens %>%
    sapply(detectR::max_from_list) %>%
    matrix(ncol = 1)

  matching <- detectR::parse_words(
    tokens = tokens,
    args = args
  )

  # Check for doubled words like `make set` etc..
  matching %<>% detectR::double_words()

  # Sapply over the 3 attack vectors
  counts <- sapply(1:(attackWords %>% length), function(x) {
    atks <- purrr::map(matching$wordList, attackWords[[x]]$has_keys)

    # Update attack tokens
    toRepl <- atks %>%
      lapply(any) %>%
      purrr::flatten_lgl() %>%
      which

    # If any exist, then replace the correct tokens
    replLen <- toRepl %>% length
    if (replLen > 0) {
      for (i in 1:replLen) {
        ind <- toRepl[i]
        matching$tokenList[[ind]][matching$indexes[[ind]][atks[[ind]]]] <<-
          if (x == 1) 'x' else if (x == 2) 'y' else 'z'
      }
    }

    # Return the attack counts as a vector of the full matrix
    atks %>%
      purrr::map(sum) %>%
      purrr::flatten_dbl()
  })
  # Create a matrix from the counts
  counts %<>% matrix(ncol = 3)

  # Shouldn't need to collapse them...
  collapse <- function(x) x %>% paste(collapse = '')
  escores <- matching$tokenList %>%
    lapply(collapse) %>%
    sapply(detectR::max_from_list) %>%
    matrix(ncol = 1)

  # Column bind the two results together
  return(cbind(scores, escores, chardist, counts))
}
