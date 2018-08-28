 #' @title Process Log File
 #'
 #' @export


process_log_file <- function(filepath) { # nocov start

  # Figure out number of lines in the file
  lineNumber <- system2(
    command = 'wc',
    args = c("-l", filepath, " | awk '{print $1}'"),
    stdout = TRUE
  ) %>%
    as.integer

  # Quick print out before going into the reading
  spltPath <- filepath %>%
    strsplit(split = '/') %>%
    purrr::flatten_chr()

  cat(
    paste0(
    " ## Reading : ", spltPath[spltPath %>% length],
    " which contains ", lineNumber, " number of lines. \n"
    )
  )

  # Read 10,000 lines at a time
  singleRead <- 10000
  numReads <- lineNumber / singleRead

  # Read an extra time to account for the singleRead leftover
  equally <- mod(lineNumber, singleRead)
  if (equally %>% `!=`(0)) numReads %<>% ceiling

  # Set a progress bar
  pb <- utils::txtProgressBar(
    min = 0,
    max = numReads,
    style = 3
  )

  # Read in now
  totalVector <- c()
  for (i in 1:numReads) {

    # Update progress bar
    utils::setTxtProgressBar(
      pb = pb,
      value = i
    )

    # Create a read connection
    con <- file(filepath, "r")

    # Figure out last iteration
    if ((equally %>% `!=`(0)) && (i %>% `==`(numReads))) singleRead <- equally

    bunchedLines <- read.csv(
      file = con,
      skip = i %>% `-`(1) %>% `*`(singleRead),
      nrow = singleRead,
      sep = '\n',
      header = FALSE,
      stringsAsFactors = FALSE
    )[ ,1]

    close(con)
    totalVector %<>% c(bunchedLines)
  }

  # Finally return the totalVector
  close(pb)
  return(
    list(
      chars = totalVector,
      fileLen = lineNumber
    )
  )
} # nocov end
