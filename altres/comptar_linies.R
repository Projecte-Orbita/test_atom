library(dplyr)
library(stringr)

# Count your lines of R code
list.files(path = ".", recursive = T, full.names = T) %>%
  str_subset("[.][R]$") %>%
  sapply(function(x) x %>% readLines() %>% length()) %>%
  sum()