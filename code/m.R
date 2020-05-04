#Nested logit coding assignment
source('/Users/yakov/Downloads/structural estimation/OPNS_NestedL/code/header.R')

set.seed(0)

#Generate data
list(
  my.buckets = 1:3, 
  my.choices = c('A', 'B', 'C')
) %>% 
  makeData %>% 
  saveRDS('/Users/yakov/Downloads/structural estimation/OPNS_NestedL/variables/values.rds')
