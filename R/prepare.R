#' @title Prepare Data
#' @export


prepare <- function() {
  # Load the large HTTP data set as d.http
  load(file = system.file("extdata", "http.rda", package = "detectR"))

  d.normal <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:(totalData %>% nrow)) {
    slice <- d.http[i, ]

    string <- if (slice$method == 'GET') slice$requestFirstLine else slice$requestBody

    if (string %>% nchar %>% `<`(1)) next

    # Get the arguments from the request method
    argsToRep <- string %>%
      strsplit(split = ' ') %>%
      purrr::map(1) %>%
      purrr::flatten_chr() %>%
      detectR::split_args()

    # If args exist then analyse them
    nRo <- argsToRep %>% nrow
    if (nRo %>% `>`(0)) {
      arguments <- argsToRep$args %>% gsub(pattern = '[+]', replacement = ' ')
      results <- c()
      #for (j in 1:nRo) results %<>% c(arguments[j] %>% detect_attacks())
      d.normal %<>% rbind(
        data.frame(
          argument = arguments,
          label = "N",
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # Save the data set
  save(d.normal, file = getwd() %>% paste0("/data/d.normal.rda"))
}
