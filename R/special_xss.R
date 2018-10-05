#' @title Special XSS
#'
#' @export


special_xss <- function(argument) {
  # Split the argument up
  splitArg <- argument %>%
    strsplit(split = "")

  arrow_counts <- function(x, arrow) x %>% purrr::map(function(z) arrow %>% `==`(z) %>% sum) %>% purrr::flatten_dbl()

  # Take pairwise minimum and check for alerts/prompts
  return(
    splitArg %>%
      arrow_counts("<") %>%
      pmin(splitArg %>% arrow_counts(">")) %>%
      `+`(argument %>% grepl(pattern = "alert\\(|prompt\\(") %>% as.integer) %>%
      as.matrix
  )
}
