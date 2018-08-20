#' @title Attack keywords
#' @export


attack_keywords <- function() {

  # Load in attack data set
  a.data <- detectR::attacks

  # Set up function for subsetting attack type
  sub_type <- function(d, t) {
    d %>%
      subset(d$type %>% `==`(t), select = "attackword") %>%
      `[[`("attackword") %>%
      as.character
  }

  # Save attack words as a list of hashmaps
  attackWords <<- list(
    SQLi = hashmap::hashmap(
      keys = a.data %>% sub_type("s"),
      values = "s" %>% rep(a.data$type %>% `==`("s") %>% sum)
    ),
    XSS = hashmap::hashmap(
      keys = a.data %>% sub_type("x"),
      values = "x" %>% rep(a.data$type %>% `==`("x") %>% sum)
    ),
    BASH =  hashmap::hashmap(
      keys = a.data %>% sub_type("b"),
      values = "b" %>% rep(a.data$type %>% `==`("b") %>% sum)
    )
  )

  # Return the list of hashmaps
  return(attackWords)
}
