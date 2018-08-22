# Load libraries in
library(magrittr, quietly = TRUE, warn.conflicts = FALSE)
library(detectR, quietly = TRUE, warn.conflicts = FALSE)
library(utils, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)

# Get the log file
fName <- getwd() %>%
  paste0("/inst/extdata/http.txt")

# Get the file contents back
fileInfo <- fName %>%
  detectR::process_log_file()

# Function for collapsing value
clpse <- function(ln) {
  return(
    ln %>%
      `[`(2 %>% `:`(ln %>% length)) %>%
      paste(collapse = ' ')
  )
}

# Initialise all data frames
totalRequest <- totalResponse <- totalData <- data.frame(stringsAsFactors = FALSE)

# Set a progress bar
pb <- utils::txtProgressBar(
  min = 0,
  max = fileInfo$fileLen,
  style = 3
)

# Loop over the log file - parse into data frame assuming a standard format
for (i in 1:fileInfo$fileLen) {

  # Update progress bar
  utils::setTxtProgressBar(
    pb = pb,
    value = i
  )

  # Read line by line
  line <- fileInfo$chars[i] %>%
    strsplit(split = ' ') %>%
    purrr::flatten_chr()

  # Get lengths
  llen <- line %>% length
  first <- line[1]

  # Match keys when parsing (GET or POST signifies start of a session)
  if (first %in% c('GET', 'POST', 'DELETE', 'PUT')) {
    # GET or post signifies start of a session

    # Make sure a previous session has been collected
    if (i %>% `!=`(1)) {
      session <- data.frame(
        method = method,
        requestFirstLine = firstLine,
        requestBody = requestBody,
        stringsAsFactors = FALSE
      )

      # set up responseHeaders
      responseHeaders <- data.frame(
        `content-type` = contentType,
        `content-length` = contentLength,
        stringsAsFactors = FALSE
      )

      # set up requestHeaders
      requestHeaders <- data.frame(
        `user-agent` = userAgent,
        `cache-control` = cacheControl,
        accept = accept,
        pragma = pragma,
        `accept-encoding` = acceptEncoding,
        `accept-charset` = acceptCharset,
        `accept-language` = acceptLanguage,
        cookie = cookie,
        connection = connection,
        host = host,
        stringsAsFactors = FALSE
      )

      # Keep binding to the total sub data frames
      totalData %<>% rbind(session)
      totalRequest %<>% rbind(requestHeaders)
      totalResponse %<>% rbind(responseHeaders)
    }
    # Set up empty strings for all possible columns
    userAgent <- cacheControl <- accept <- pragma <-
      acceptEncoding <- acceptCharset <- acceptLanguage <-
      cookie <- connection <- host <- requestBody <-
      contentLength <- contentType <- ""

    # Get the method and first line from this indicator
    method <- first
    firstLine <- line %>% clpse()

  } else if (first %>% `==`('User-Agent:')) {
    userAgent <- line %>% clpse()
  } else if (first %>% `==`('Cache-control:')) {
    cacheControl <- line %>% clpse()
  } else if (first %>% `==`('Accept:')) {
    accept <- line %>% clpse()
  } else if (first %>% `==`('Pragma:')) {
    pragma <- line %>% clpse()
  } else if (first %>% `==`('Accept-Encoding:')) {
    acceptEncoding <- line %>% clpse()
  } else if (first %>% `==`('Accept-Charset:')) {
    acceptCharset <- line %>% clpse()
  } else if (first %>% `==`('Content-Type:')) {
    contentType <- line %>% clpse()
  } else if (first %>% `==`('Accept-Language:')) {
    acceptLanguage <- line %>% clpse()
  } else if (first %>% `==`('Cookie:')) {
    cookie <- line %>% clpse()
  } else if (first %>% `==`('Connection:')) {
    connection <- line %>% clpse()
  } else if (first %>% `==`('Host:')) {
    host <- line %>% clpse()
  } else if (first %>% `==`('Content-Length:')) {
    contentLength <- line %>% clpse()
  } else if (first %>% `==`(line)) {
    if (line %>% length %>% `>`(1)) print(line)
    requestBody <- line
  }
}

# Close the progress bar
close(pb)

# Bind the headers to the full data frame
totalData$requestHeaders <- totalRequest
totalData$responseHeaders <- totalResponse

# Save the rda to the same directory
fName %<>%
  strsplit(split = '[.]') %>%
  purrr::map(1) %>%
  purrr::flatten_chr()

save(totalData, file = paste0(fName, '.rda'))
