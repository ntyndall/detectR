#' @title Subset Empty
#'
#' @export


subset_empty <- function(z) {
  z %<>% subset(z != "")
  return(if (z %>% length %>% `>`(0)) z else "")
}
