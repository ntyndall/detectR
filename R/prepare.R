#' @title Prepare Data
#' @export


prepare <- function() {
  # Load the large HTTP data set as d.http
  load(file = system.file("extdata", "d.http.rda", package = "detectR"))

  # Logical vector
  lgc <- d.http$method %>% `==`("GET")

  # Loop over query args and request bodies
  d.normal <- data.frame(stringsAsFactors = FALSE)
  for (i in 1:2) {
    strs <- if (i == 1) {
      d.http %>%
        subset(lgc, select = "requestFirstLine") %>%
        `[[`("requestFirstLine")
    } else {
      "www.test.com/?" %>%
        paste0(
          d.http %>%
            subset(lgc %>% `!`(), select = "requestBody") %>%
            `[[`("requestBody")
        )
    }

    # Only take the supplied arguments
    arguments <- strs %>%
      strsplit(split = ' ') %>%
      purrr::map(1) %>%
      purrr::flatten_chr() %>%
      detectR::split_args() %>%
      `[[`("args") %>%
      gsub(pattern = '[+]', replacement = ' ')

    # Append to data frame
    d.normal %<>% rbind(
      data.frame(
        argument = arguments,
        label = "N",
        stringsAsFactors = FALSE
      )
    )
  }

  # Save the data set
  save(d.normal, file = getwd() %>% paste0("/data/d.normal.rda"))
}
