#' @title Attack Counts
#'
#' @export


attack_counts <- function(matching, attackWords) {
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
        aType <- if (x == 1) 'x' else if (x == 2) 'y' else 'z'
        for (i in 1:replLen) {
          j <- toRepl[i]
          matching[[j]]$tokenList[matching[[j]]$indexes[atks[[j]]]] <- aType
        }
      }

      # Return the attack counts as a vector of the full matrix
      atks %>%
        purrr::map(sum) %>%
        purrr::flatten_dbl()
    }
  )

  # Create a matrix from the counts and return back
  return(
    list(
      counts = counts %>% matrix(ncol = 3),
      matched = matching
    )
  )
}
