library(detectR, warn.conflicts = F, quietly = T)
library(magrittr, warn.conflicts = F, quietly = T)


# Make sure the normal data set is built
fName <- getwd() %>% paste0("/data/d.normal.rda")

# Build it, and load it
d.normal <- if (fName %>% file.exists) {
  detectR::d.normal
} else {
  detectR::prepare()
  load(file = fName)
}

# Load in all data sets
d.set <- rbind(
  d.normal,
  detectR::d.sqli,
  detectR::d.xss,
  detectR::d.bash
)

# Build the feature sets
d.features <- d.set$argument %>%
  detectR::features()

# Append the labels onto the data set
d.features$label <- d.set$label

emptyFeats <- d.features %>% apply(
  MARGIN = 2,
  FUN = function(x) x %>% unique %>% length
) %>% as.double

if (emptyFeats %>% `==`(1) %>% any) {
  d.features %<>% subset(select = names(d.features) %>% `[`(emptyFeats %>% `!=`(1) %>% which))
}

# Build the model ...
results <- d.features %>%
  detectR::builder(
    normalData = 2000,
    percent = 80
  )

# ... and save it
save(results$nn, file = getwd() %>% paste0("/data/nn.rda"))
save(results$dataScales, file = getwd() %>% paste0("/data/dataScales.rda"))
