library(excessmort)
library(lubridate)
library(tidyverse)

data("puerto_rico_counts")

puerto_rico_counts_est <-  puerto_rico_counts %>%
  filter(month(date)==7 , day(date)==1) %>%
  mutate(agegroup = as.character(agegroup)) %>%
  mutate(agegroup2 = ifelse(agegroup %in% c("85-89", "90-94", "95-99", "100-Inf"),  
                            "85Inf", 
                            agegroup)) %>%
  group_by(year(date), agegroup2) %>%
  summarise(pop = sum(population)) %>%
  mutate(agegroup = factor(agegroup2, levels = ageRange_levels))

### 1990-2000
## RIP

### 2000-2010
### date variable description: https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.2000-2010_Intercensals.html
  
### 2011-2013 
### https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2013.html#list-tab-794389051

source("Census_function.R")
source("census_key.R")

#### SANITY CHECK -- comparison to excessmort


test_3 <- make_tidy_pop_estimates(year = c(2002,2015,2020:2021), 
                           municipio = T, 
                           census_key = census_key)

test_3_new <- make_tidy_pop_estimates(year = c(2015,2020:2021), 
                                     municipio = T,
                                     census_key = census_key)

prueba <- test_3 %>%
 group_by(year) %>%
  summarise(max_age=max(age))

# sum(prueba$estimate)

ageRange_starts <- c( seq(0, 85, 5))
ageRange_ends <- c(ageRange_starts[-1]-1, Inf)
ageRange_levels <- paste(ageRange_starts, ageRange_ends, sep = "-")
ageRange_levels[length(ageRange_levels)] <- paste0(ageRange_starts[length(ageRange_levels)],"Inf")

sum_sya <- test_3 %>% 
  mutate(year=as.numeric(year))%>%
  #mutate(age2 = ifelse(age == 0, 1, age)) %>%
 # mutate(age2 = ifelse(age2>=85, 86, age2)) %>%
  mutate(agegroup = cut(age, c(0, ageRange_ends), labels = ageRange_levels, right = F, include.lowest=T)) %>%
  group_by(year, agegroup) %>%
  summarise(pop_sya = sum(estimate))

verification <- right_join(puerto_rico_counts_est, sum_sya,
                           by=c("year(date)"="year",  "agegroup")) %>% 
  mutate(diff = pop-pop_sya) %>%
  select(agegroup, `year(date)`, pop, pop_sya) %>%
  setNames(c("agegroup", "year", "excessmort", "censusdata")) %>%
  pivot_longer(cols = c("excessmort", "censusdata"))

ggplot( verification, aes(x = year, y = value, color = name)) + geom_line() + 
  facet_wrap(vars(agegroup), scales = "free_y")

### comparison to united nations data ###

source("united_nations_data.R")

un_20_21 <- df_UNPD %>%
  mutate(year = as.numeric(timeLabel)) %>%
  mutate(age = as.numeric(ageStart)) %>%
  filter(year >= 2000, year <=2021, sex != "Both sexes") %>%
  mutate(sex = recode(sex, `Female`="F", `Male`="M")) %>%
  mutate(age = ifelse(age>=85, 85, age)) %>%
  mutate(age = cut(age, c(0, ageRange_ends), labels = ageRange_levels, right = F, include.lowest=T)) %>%
  select(age, sex, value, year) %>%
  setNames(c("age","gender","estimate_un","year")) %>%
  group_by(age,gender,year) %>%
  summarise(estimate_un=sum(estimate_un)) %>%
  ungroup()

census_OG <- make_tidy_pop_estimates(year = c(2019, 2020), 
                                 municipio = F,
                                 census_key = census_key)

census_20_21 <- census %>%
  mutate(age = ifelse(age>=85, 85, age)) %>%
  mutate(age = cut(age, c(0, ageRange_ends), labels = ageRange_levels, right = F, include.lowest=T)) %>%
  group_by(age,gender,year) %>%
  summarise(estimate=sum(estimate)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))

comparison <- merge(un_20_21, census_20_21, by=c("age", "gender", "year"))  %>%
  mutate(difference_un_cb = (estimate-estimate_un)/estimate)

ggplot(comparison, aes(x=year, y = difference_un_cb)) +
  geom_hline(yintercept=0, color="red", alpha =0.5)+
  geom_line() +
  facet_grid(rows=vars(age), cols = vars(gender)) + ylim(-0.2,0.2)



### only UN ###

un_data <- make_tidy_pop_estimates(year = c(2000:2021), 
                        municipio = FALSE,
                        group = FALSE, 
                        un_data = TRUE,
                        table_type = NULL, 
                        census_key = census_key)

census_data <- make_tidy_pop_estimates(year = c(2017:2017), 
                                        municipio = FALSE,
                                        un_data = FALSE,
                                        census_key = census_key)


un_data_sum <- un_data %>%
  group_by(year) %>%
  summarise(estimate = sum(estimate))
census_data_sum <- census_data %>%
  group_by(year) %>%
  summarise(estimate = sum(estimate))

comparison_2 <- merge(un_data_sum, census_data_sum, by = "year") %>%
  mutate(difference = estimate.x - estimate.y)

census_key <- "5f45609e8b5af7804d7ba5f3262c056df558488a"
### projections ###

un_data_projections <- make_tidy_pop_estimates(year = c(2016,2022), 
                                   municipio = T,
                                   un_data = F,
                                   census_key = census_key)

pop_by_age_gender <- make_tidy_pop_estimates(year = c(2022), 
                           municipio = F,
                           un_data = F,
                           census_key = census_key)

save(pop_by_age_gender, file = "~/Documents/PhD/bivalent_booster/rdas/pop_tabs.rda")
  