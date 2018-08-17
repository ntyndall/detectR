#' @title Tokenization
#' @export


tokenization <- function(stringToToken) {
  stringToToken %<>%
    gsub(pattern = "[\a\f\t\v\b\r\n]+", replacement = "") %>%
    gsub(pattern = "[[:alpha:]]+", replacement = "w") %>%
    gsub(pattern = "[[:digit:]]+", replacement = "n") %>%
    gsub(pattern = "[[:blank:]]+", replacement = "s") %>%
    gsub(pattern = "(\\.\\./)+", replacement = "d") %>%
    gsub(pattern = "[()]+", replacement = "p") %>%
    gsub(pattern = "[[]+", replacement = "b") %>%
    gsub(pattern = "[]]+", replacement = "b") %>%
    gsub(pattern = "[*]+", replacement = "a") %>%
    gsub(pattern = "[{}]+", replacement = "c") %>%
    gsub(pattern = "[.]+", replacement = "f") %>%
    gsub(pattern = "[!&|]+", replacement = "o") %>%
    gsub(pattern = "['\"]+", replacement = "t") %>%
    gsub(pattern = "[+-=/]+", replacement = "j") %>%
    gsub(pattern = "[[:punct:]]+", replacement = "l") %>%
    return()
}
