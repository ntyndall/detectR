#' @title ICDist
#'
#' @description A function that compares a requests character distribution with
#'  normal data.
#'
#' @details A character distribution is created and compared against an Idealised
#'  Character Distribution (ICD) using a Kolmogorov–Smirnov test.
#'
#' @param argument The argument/query supplied to the field.
#'
#' @return Returns a binary flags for the anomaly check. "1" for anomalous,
#'  "0" for normal.
#'
#' @keywords anomaly request query argument
#'
#' @export


icdist <- function(argument) {
  distr <- argument %>%
    strsplit(split = '') %>%
    purrr::flatten_chr() %>%
    table %>%
    as.data.frame %>%
    `[[`('Freq') %>%
    sort(decreasing = TRUE)

  return(
    retdata <- .Fortran(
      detectR:::distval,
      values = distr %>% as.integer,
      vecLength = distr %>% length %>% as.integer,
      icd = 6 %>% integer %>% as.integer
    )$icd
  )
}
