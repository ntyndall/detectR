#' @title Token hash
#'
#' @param elevate A character string which is one of;
#'  \itemize{
#'    \item{\code{N} : To indicate a normal classified string.}
#'    \item{\code{X} : To indicate a SQLi classified string.}
#'    \item{\code{Y} : To indicate a XSS classified string.}
#'    \item{\code{Z} : To indicate a Bash classified string.}
#'  }
#'
#' @export


token_hash <- function(elevate) { # nocov start
  # Load Current token scores
  t.scores <- detectR::t.scores

  # Return the correct hashmap
  return(
    hashmap::hashmap(
      keys = t.scores$tokens,
      values = t.scores %>% `[[`(elevate) %>% as.integer
    )
  )
} # nocov end
