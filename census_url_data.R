library(openxlsx)
library(tidyverse)
library(janitor)

popest_sya_url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2021/puerto-rico/asrh/prc-est2021-syasex.xlsx"

popest_sya_tidy <- read.xlsx(popest_sya_url, 
                             startRow = 4, rows = c(1:93))%>%
  select(-X2, -X3, -X4, -`2020`, -`2021`) %>%
  setNames(c("age", 
             "MALE_2020", "FEMALE_2020",  
             "MALE_2021", "FEMALE_2021")) %>%
  filter(!(age %in% c("Total")) & !is.na(age)) %>%
  mutate(age = str_remove(age, "."), 
         age = str_remove(age, "\\+")) %>%
  pivot_longer(cols = c(-age), names_to = "gender", values_to = "estimate") %>%
  separate(gender, c("gender","year")) %>%
  mutate(gender = recode(gender, MALE = "M", FEMALE = "F"))
  
popest_muni_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-72.csv"

popest_muni_tidy <- read.csv(popest_muni_url, 
                             fileEncoding = "ISO-8859-1") %>%
  select(NAME, YEAR, matches("AGE.+_[FEM|MALE]")) %>%
  rename(municipio = "NAME", year = "YEAR") %>%
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
  select(municipio, start, end, gender, poblacion) 

