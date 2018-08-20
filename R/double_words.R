#' @title Double words
#' @export


double_words <- function(matching) {

  # Define double word combinations
  dbls <- detectR::doubles$doublewords %>%
    as.character %>%
    strsplit(split = " ")

  dL <- dbls %>% length
  wList <- matching$wordList
  lens <- wList %>%
    purrr::map(length) %>%
    purrr::flatten_dbl()

  # Only modify those that need modified
  dblInd <- lapply(
    X = 1:(lens %>% length),
    FUN = function(x) {
      dblMatch <- sapply(
        X = dbls,
        FUN = function(y) y %in% wList[[x]] %>% all
      )
      if (lens[x] > 1 && dblMatch %>% any) dblMatch %>% which else NULL
    }
  ) %>%
    purrr::flatten_dbl()

  # Create logical vector
  tr <- dblInd %>%
    purrr::map(function(z) z %>% is.null %>% `!`()) %>%
    purrr::flatten_lgl()

  # Update any matches with the double match
  if (tr %>% any) {
    toUpdate <- tr %>% which
    for (i in 1:(toUpdate %>% length)) {
      ind <- toUpdate[i]

      # Matching double barrel name
      doubleMatch <- dbls %>% `[[`(dblInd %>% `[[`(ind))

      # Get the matched double barrel
      dMatch <- doubleMatch %>% match(matching$wordList[[ind]])

      wL <- matching$wordList[[ind]]
      iL <- matching$indexes[[ind]]

      doubleSplit <- doubleMatch
      doubleMatch %<>% paste(collapse = '')

      # Collapse the token down
      rm <- iL[dMatch]
      splt <- matching$tokenList[[ind]]
      splt %<>% `[`(c(1:(splt %>% length))[-c(rm[1]:(rm[2] - 1))])
      if (rm[2] > splt %>% length) splt[rm[1]] <- 'w' else splt[rm[2]] <- 'w'
      matching$tokenList[[ind]] <- splt

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
      matching$wordList[[ind]] <- update$wordList
      matching$indexes[[ind]] <- update$indexes
    }
  }
  return(matching)
}
