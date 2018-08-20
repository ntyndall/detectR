#' @title Tokenization attributes
#' @export


features <- function(args) {

  # Initialise attack keywords
  attackWords <- detectR::attack_keywords()
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

  # Define double word combinations
  dbls <- detectR::doubles$doublewords %>%
    as.character %>%
    strsplit(split = " ")

  # Check for doubled words like `make set` etc..
  matching %<>% lapply(
    FUN = function(x) x %>% double_words(dbls = dbls)
  )

  # Sapply over the 3 attack vectors
  counts <- sapply(
    X = 1:(attackWords %>% length),
    FUN = function(x) {
      atks <- matching %>%
        purrr::map("wordList") %>%
        purrr::map(attackWords[[x]]$has_keys)

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
          matching[[ind]]$tokenList[matching[[ind]]$indexes[atks[[ind]]]] <<-
            if (x == 1) 'x' else if (x == 2) 'y' else 'z'
        }
      }

      # Return the attack counts as a vector of the full matrix
      atks %>%
        purrr::map(sum) %>%
        purrr::flatten_dbl()
    }
  )
  # Create a matrix from the counts
  counts %<>% matrix(ncol = 3)

  # Shouldn't need to collapse them (but keep for now)
  escores <- matching %>%
    purrr::map("tokenList") %>%
    lapply(function(x) x %>% paste(collapse = ''))
    sapply(detectR::max_from_list) %>%
    matrix(ncol = 1)

  # Column bind the two results together
  return(cbind(scores, escores, chardist, counts))
}
