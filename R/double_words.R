#' @title Double words
#' @export


double_words <- function(matching) {
  dbls <- list(c('or', 'not'), c('make', 'set'), c('case', 'when'), c('all', 'users'),
               c('user', 'lock'), c('generate', 'series'), c('wait', 'for'))
  dL <- dbls %>% length
  wList <- matching$wordList
  lens <- wList %>%
    purrr::map(length) %>%
    purrr::flatten_dbl()

  # Only modify those that need modified
  dblInd <- sapply(1:(lens %>% length), function(y) {
    dblMatch <- sapply(1:dL, function(x) dbls[[x]] %in% wList[[y]] %>% all)
    if (lens[y] > 1 && dblMatch %>% any) dblMatch %>% which else NULL
  })

  # Create logical vector
  tr <- dblInd %>%
    purrr::map(function(z) ! is.null(z)) %>%
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
        # There are three conditions here?
        bf <- function() c(1:(dMatch[1] - 1))
        af <- function() c((dMatch[2] + 1):vecLen)

        if (wL[c(1, 2)] %>% `==`(doubleSplit) %>% all) {
          list(
            wordList = c(doubleMatch, wL[af()]),
            indexes = c(dMatch[1], iL[af()] %>% `-`(2))
          )
        } else if (wL[c(vecLen - 1, vecLen)] %>% `==`(doubleSplit) %>% all) {
          list(
            wordList = c(wL[bf()], doubleMatch),
            indexes = c(iL[bf()], dMatch[2])
          )
        } else {
          list(
            wordList = c(wL[bf()], doubleMatch, wL[af()]),
            indexes = c(iL[bf()], iL[dMatch[1]], iL[af()] %>% `-`(2))
          )
        }
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
