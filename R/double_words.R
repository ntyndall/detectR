#' @title Double words
#' @export


double_words <- function(single, dbls) {

  # Loop over all words
  dL <- dbls %>% length
  wList <- single$wordList
  lens <- wList %>%
    purrr::map(length) %>%
    purrr::flatten_dbl()

  # Only modify those that need modified
  dblInd <- lapply(
    X = 1:(dbls %>% length),
    FUN = function(x) {
      if (wList %>% length %>% `>`(1) %>% `&`(dbls[[x]] %in% wList %>% all)) x else NULL
    }
  ) %>%
    purrr::flatten_dbl()

  # Update any matches with the double match
  if (dblInd %>% length %>% `>`(0)) {
    for (i in 1:(dblInd %>% length)) {
      # Matching double barrel name
      doubleMatch <- dbls %>%
        `[[`(dblInd %>% `[[`(i))

      # Get the matched double barrel
      dMatch <- doubleMatch %>%
        match(single$wordList)

      wL <- single$wordList
      iL <- single$indexes

      doubleSplit <- doubleMatch
      doubleMatch %<>% paste(collapse = '')

      # Collapse the token down
      rm <- iL[dMatch]
      splt <- single$tokenList
      splt %<>% `[`(c(1:(splt %>% length))[-c(rm[1]:(rm[2] - 1))])
      if (rm[2] > splt %>% length) splt[rm[1]] <- 'w' else splt[rm[2]] <- 'w'
      single$tokenList <- splt

      # Create a vector of other indexes
      vecLen <- wL %>% length
      allInd <- c(1:vecLen)

      # Make sure the diff in indexes is always 1
      includeInd <- setdiff(allInd, dMatch)
      update <- if (includeInd %>% length %>% `>`(0)) {
        vecLog <- wL %>% `==`(doubleSplit)
        inds <- vecLog %>% which
        wL %<>%
          `[`(-inds) %>%
          append(values = doubleMatch, after = inds[1] - 1)

        # Subtract by two towards the end
        if (inds[2] %>% `<`(iL %>% length)) iL[(inds[2] + 1):(iL %>% length)] %<>% `-`(2)
        iL <- c(iL %>% `[`(!vecLog), iL %>% `[`(inds[1])) %>% sort

        list(wordList = wL, indexes = iL)
      } else {
        list(wordList = doubleMatch, indexes = dMatch[1])
      }

      # Update lists here
      single$wordList <- update$wordList
      single$indexes <- update$indexes
    }
  }
  return(single)
}
