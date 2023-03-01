library(excessmort)
library(lubridate)
library(tidyverse)

data("puerto_rico_counts")

puerto_rico_counts_est <-  puerto_rico_counts %>%
  filter(month(date)==7 , day(date)==1) %>%
  group_by(year(date)) %>%
  summarise(pop = sum(population))

### 1990-2000
## RIP

### 2000-2010
### date variable description: https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.2000-2010_Intercensals.html
  
### 2011-2013 
### https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2013.html#list-tab-794389051

source("Census_function.R")
source("census_key.R")

#### SANITY CHECK -- comparison to excessmort


test_3 <- make_tidy_census(year = c(2000:2021), 
                           municipio = F,
                           group = F, 
                           table_type = NULL, 
                           census_key = census_key)



sum_sya <- test_3 %>% 
  mutate(year=as.numeric(year))%>%
  group_by(year) %>%
  summarise(pop_sya = sum(estimate))

verification <- right_join(puerto_rico_counts_est, sum_sya,
                           by=c("year(date)"="year")) %>% 
  mutate(diff = pop-pop_sya)







