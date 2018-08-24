#' @title Split Args
#'
#' @description A function that splits URL parameters into parameter value pairs,
#'  and the length of the query strings.
#'
#' @param urls An array of URL strings. These will be URL-encoded but httr will
#'  decode these in the lappy function
#'
#' @return A data frame containing all of the argument names, values, and character
#'  lengths.
#'
#' @export


split_args <- function(urls) {
  parameters <- lapply(
    X = urls,
    FUN = function(x) httr::parse_url(x)$query
  )

  # Get initial lengths of parameters per query
  argumentPerQueryLength <- parameters %>%
    purrr::map(length) %>%
    as.integer

  # Flatten the list now
  parameters %<>% purrr::flatten()
  if (parameters %>% length %>% `==`(0)) return(data.frame())

  # Subset the required ID locations
  ids <- which(argumentPerQueryLength > 0)
  mongoIDs <- ids %>%
    rep(argumentPerQueryLength %>% `[`(argumentPerQueryLength != 0))

  # Convert to character vectors
  argumentNames <- parameters %>% names
  arguments <- parameters %>% as.character
  encoding <- stringi::stri_enc_mark(arguments)

  # Convert to ASCII.. remove any non-ascii characters for now
  if (any(encoding != 'ASCII')) {
    nonASCII <- which(!(encoding %in% 'ASCII'))
    parsedVector <- sapply(
      X = nonASCII,
      FUN = function(z) iconv(x = arguments[z], from = encoding[z], to = 'ASCII', sub = '')
    )
    arguments[nonASCII] <- parsedVector
  }

  return(
    data.frame(
      argNames = argumentNames,
      args = arguments,
      len = arguments %>% nchar,
      mongoID = mongoIDs,
      stringsAsFactors = FALSE
    )
  )
}
