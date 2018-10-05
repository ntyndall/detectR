#' @title Special Bash
#'
#' @export


special_bash <- function(argument) {

  # Convert argument with appropriate bash rules and exceptions
  argument %<>%
    gsub(pattern = "(\\.\\./)+", replacement = " dtdtslsh ") %>%
    gsub(pattern = "(\\$\\d+ )", replacement = " scptrgs ") %>%
    gsub(pattern = " && | \\|\\| ", replacement = " lgiclop ") %>%
    strsplit(split = "[[:punct:][:blank:]]+") %>%
    purrr::flatten_chr()

  # Return number of special bash characters here
  return(argument %in% c("dtdtslsh", "scptrgs", "lgiclop") %>% sum)
}
