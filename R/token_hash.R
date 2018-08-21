#' @title Token hash
#' @export


token_hash <- function(elevate) { # start nocov
  # Load Current token scores
  t.scores <- detectR::t.scores

  # Return the correct hashmap
  return(
    hashmap::hashmap(
      keys = t.scores$tokens,
      values = t.scores %>% `[[`("elevate") %>% as.integer
    )
  )
} # end nocov
