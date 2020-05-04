my.library <- '/Users/yakov/Documents/R/win-library/3.6'
.libPaths(my.library)

library('tidyverse')
c('reshape2', 'magrittr', 'sqldf','rsample','aod', 'fExtremes','boot','car') %>%
  walk(~library(., character.only=TRUE))

dir('modules') %>% 
  walk(~source(paste('./modules/', ., sep="")))

set.seed(1)
