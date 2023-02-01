library(openxlsx)
library(tidyverse)
library(janitor)

### First we extract data from recent years, 2020 and 2021,
### which are not available through api

popest_sya_url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2021/puerto-rico/asrh/prc-est2021-syasex.xlsx"

popest_sya_tidy <- read.xlsx(popest_sya_url,
                             fillMergedCells = T, startRow = 5, check.names = T,
                             rows = c(1:93)) %>%
  select(X1, matches(".1|.2"), -matches("total")) %>%
  rename(age = X1) %>%
  rename_with(~gsub(pattern = "\\.2", replacement = "_2021", .x)) %>%
  rename_with(~gsub(pattern = "\\.1", replacement = "_2020", .x)) %>%
  filter(!(age %in% c("Total")) & !is.na(age)) %>%
  mutate(age = str_remove(age, "."),
         age = str_remove(age, "\\+")) %>%
  pivot_longer(cols = c(-age), names_to = "gender", values_to = "estimate") %>%
  separate(gender, c("gender","year")) %>%
  mutate(gender = recode(gender, Male = "M", Female = "F")) %>%
  mutate(estimate = as.numeric(estimate),
         age = as.numeric(age))

popest_muni_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-72.csv"

popest_muni_tidy <- read.csv(popest_muni_url,
                             fileEncoding = "ISO-8859-1") %>%
  select(NAME, YEAR, matches("AGE.+_[FEM|MALE]")) %>%
  rename(municipio = "NAME", year = "YEAR") %>%
  filter(year != 1) %>%
  mutate(year = recode(as.character(year),  `2` = "2020", `3` = "2021")) %>%
  mutate(municipio = str_remove(municipio, " Municipio")) %>%
  pivot_longer(cols = c(-municipio, -year)) %>%
  mutate(name = str_remove(name, "AGE")) %>%
  separate(name, c("age","gender")) %>%
  mutate(age = recode(age, `04` = "0004", `59` = "0509",`513`= "0513",
                      `85PLUS` = "85Inf")) %>%
  pivot_wider(names_from = age, values_from = value) %>%
  select(municipio, year, gender, "0004", "0509", "1014", "1519", "2024", "2529",
         "3034", "3539", "4044","4549", "5054", "5559", "6064", "6569", "7074",
         "7579", "8084", "85Inf") %>%
  filter(!(gender %in% c("TOT"))) %>%
  mutate(gender = recode(gender, MALE = "M", FEM = "F")) %>%
  pivot_longer(-c("municipio","gender", "year"), names_to = "age", values_to = "poblacion") %>%
  mutate(start = as.numeric(str_extract(age, "\\d{2}")),
         end = as.numeric(str_remove(age, "\\d{2}"))) %>%
  select(municipio, start, end, gender, poblacion, year)


#sum(popest_muni_tidy[popest_muni_tidy$year == "2020", ]$poblacion)
#sum(popest_sya_tidy[popest_sya_tidy$year=="2020", ]$estimate)


### previous years 2015 - 2019 

source("Census_function.R")
source("census_key.R")


pop_est_sya_2015_2019 <- map_df(c(2015:2019), function(y){
  tmp <- get_census_data(product = "pep", 
                         subproduct = "charage",
                         year = y, 
                         variables = c("AGE", "SEX", "POP"), 
                         group = F, 
                         table_type = NULL, 
                         census_key = census_key)
  tmp <- tmp[[1]]
  tmp$year <- as.character(y)
  return(tmp)
})


### By municipios 


pop_est_muni_2015_2019 <- map_df(c(2015:2019), function(y){
  tmp <- get_census_data(product = "pep", 
                         subproduct = "charagegroups",
                         year = y, 
                         variables = c("AGEGROUP", "SEX", "GEONAME", "POP"), 
                         municipio = T,
                         group = F, 
                         table_type = NULL, 
                         census_key = census_key)
  tmp <- tmp[[1]]
  tmp$year <- as.character(y)
  return(tmp)
})

### now we merge everything to obtain tidy data sets of population estimates for 
### Puerto Rico from 2015-2021

pop_est_sya <- pop_est_sya_2015_2019 %>%
  select(AGE, SEX, POP, year) %>%
  setNames(c("age", "gender", "estimate", "year")) %>%
  filter(gender %in% c("1", "2"), !(age %in% c("999"))) %>%
  mutate(gender = recode(gender, `1` = "M", `2` = "F")) %>%
  mutate(age = as.numeric(age), 
         estimate = as.numeric(estimate)) %>%
  full_join(popest_sya_tidy, by = c("age", "gender", "estimate", "year"))

pop_est_muni <- pop_est_muni_2015_2019 %>%
  select(-state, -county) %>%
  setNames(c("agegroup", "gender", "municipio", "poblacion", "year")) %>%
  mutate(agegroup = as.numeric(agegroup)) %>%
  filter(agegroup %in% c(1:18), gender %in% c("1", "2")) %>%
  mutate(gender = recode(gender, `1` = "M", `2` = "F")) %>%
  mutate(agegroup = case_when(agegroup == 1 ~ "0004", agegroup == 2 ~ "0509", 
                              agegroup == 3 ~ "1014", agegroup == 4 ~ "1519", 
                              agegroup == 5 ~ "2024", agegroup == 6 ~ "2529", 
                              agegroup == 7 ~ "3034", agegroup == 8 ~ "3539", 
                              agegroup == 9 ~ "4044", agegroup == 10 ~ "4549", 
                              agegroup == 11 ~ "5054", agegroup == 12 ~ "5559", 
                              agegroup == 13 ~ "6064", agegroup == 14 ~ "6569", 
                              agegroup == 15 ~ "7074", agegroup == 16 ~ "7579", 
                              agegroup == 17 ~ "8084", agegroup == 18 ~ "85Inf")) %>%
  mutate(start = as.numeric(str_extract(agegroup, "\\d{2}")),
         end = as.numeric(str_remove(agegroup, "\\d{2}"))) %>%
  mutate(municipio = str_remove(municipio, " Municipio, Puerto Rico")) %>% 
  select(-agegroup) %>% 
  mutate(poblacion = as.numeric(poblacion)) %>%
  full_join(popest_muni_tidy)

## sanity check ##

sum_muni <- pop_est_muni %>% 
  group_by(year) %>%
  summarise(pop_muni = sum(poblacion))
sum_sya <- pop_est_sya %>% 
  group_by(year) %>%
  summarise(pop_sya = sum(estimate))

verification <- merge(sum_muni, sum_sya, by="year")

