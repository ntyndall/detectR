#' @title Word In Vector
#'
#' @export


word_in_vec <- function(x) return(if ('w' %in% x) which('w' == x) else NULL)
