#' @title Feature Names
#'
#' @export


feat_names <- function() {
  return(
    c("rawscore", "elevatedscore", paste0("dist_", 1:6), paste0("atk_", c("sqli", "xss", "bash")))
  )
}
