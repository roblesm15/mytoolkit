#### Pulling Puerto Rico Census Data from APIs ####

## product <- acs1 ; acs3, acs5,  
### product details here: https://api.census.gov/data.html
## acs1 only provides estimates for counties where population >= 65,000
## acs5 https://www.census.gov/data/developers/data-sets/acs-5year.html


get_un_data <- function(start_year, end_year){
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)
  library(purrr)
  
  base_url_UNPD <- "https://population.un.org/dataportalapi/api/v1"
  
  puerto_rico_iso3 <- "PRI"
  puerto_rico_iso2 <- "PR"
  puerto_rico_id <- "630"
  indicator_code <- "47"
  start_year <- start_year
  end_year <- end_year
  
  target <- paste0(base_url_UNPD, "/data/indicators/", indicator_code, 
                   "/locations/", puerto_rico_id, "/start/", start_year, "/end/", end_year)
  
  #call the API 
  response <- fromJSON(target)
  
  #get the table with data available in the first page
  df_UNPD <- response$data
  
  #until there are next pages available
  while (!is.null(response$nextPage)){
    #call the API for the next page
    response <- fromJSON(response$nextPage)
    #add the data of the new page to the data.frame with the data of the precious pages
    df_UNPD <- rbind(df_UNPD, response$data)
  }
  return(df_UNPD)
}

get_census_data <- function(product, subproduct, year, variables, municipio = F, group = F, 
                            table_type = NULL, census_key) {
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)
  library(purrr)
  
  if (any(year <= 2014) & !municipio){
    year_new <- year[year<=2014]
    if (any(year_new %in% c(2000:2009))){
      x<- GET(paste0("http://api.census.gov/data/2000/pep/int_charage?get=POP,SEX,AGE,DATE_&for=state:72&key=", census_key))
      
      data_census_x <- fromJSON(rawToChar(x$content), flatten=T) %>% 
        as.data.frame() %>%
        row_to_names(1) %>%
        filter(AGE != 999, !(DATE_ %in% c("1","12")), SEX != 0) %>%
        mutate(date = as.numeric(DATE_)) %>%
        mutate(year = case_when(date == 2 ~ 2000, 
                                date == 3 ~ 2001,
                                date == 4 ~ 2002,
                                date == 5 ~ 2003, 
                                date == 6 ~ 2004,
                                date == 7 ~ 2005,
                                date == 8 ~ 2006, 
                                date == 9 ~ 2007, 
                                date == 10 ~ 2008, 
                                date == 11 ~ 2009)) %>%
        select(AGE, SEX, POP, year) %>%
        setNames(c("age", "gender", "estimate", "year")) %>%
        filter(year %in% year_new) %>%
        mutate(gender = recode(gender, `1` = "M", `2` = "F")) %>%
        mutate(age = as.numeric(age),
               estimate = as.numeric(estimate))
    } else {data_census_x = NULL}
    
    if (any(year_new %in% c(2010:2014))){
      y <- GET(paste0("https://api.census.gov/data/2014/pep/prcagesex?get=POP,SEX,STNAME,AGE,DATE_&for=state:72&key=", census_key))
      
      data_census_y<- fromJSON(rawToChar(y$content), flatten=T) %>% 
        as.data.frame() %>%
        row_to_names(1) %>%
        filter(AGE != 999, !(DATE_ %in% c("1","2")), SEX != 0) %>%
        mutate(date = as.numeric(DATE_)) %>%
        mutate(year = case_when(date == 3 ~ 2010,
                                date == 4 ~ 2011,
                                date == 5 ~ 2012, 
                                date == 6 ~ 2013, 
                                date == 7 ~ 2014)) %>%
        select(AGE, SEX, POP, year) %>%
        setNames(c("age", "gender", "estimate", "year")) %>%
        mutate(gender = recode(gender, `1` = "M", `2` = "F")) %>%
        filter(year %in% year_new) %>%
        mutate(age = as.numeric(age),
               estimate = as.numeric(estimate))
    } else {data_census_y = NULL}
    
    data_census <- list(rbind(data_census_x, data_census_y))
    variables_information = NULL
    
  }
  
  if (any(year > 2014)){
    year = year[year>2014]
    if (year == 2018 & product == "pep" & municipio == FALSE){
      url <- paste0("https://api.census.gov/data/2018/pep/charage?get=AGE,SEX,POP&DATE_CODE=11&for=state:72&key=",
                    census_key)
      data_census <- GET(url)
      data_census <- fromJSON(rawToChar(data_census$content), flatten=T) %>% 
        as.data.frame() %>%
        row_to_names(1)
      variables_information <- NULL
    } else {
      
      url_base <- ifelse(is.null(table_type), 
                         paste0(c("http://api.census.gov/data", year, product, 
                                  subproduct), collapse = "/"), 
                         paste0(c("http://api.census.gov/data", year, product, 
                                  subproduct, table_type), collapse = "/"))
      
      if (group){ 
        url <- ifelse(product == "pep", 
                      paste0(url_base, "?get=", variables, "&for=state:72&key=", census_key),
                      paste0(url_base, "?get=group(", variables,")&for=county:*&in=state:72&key=", 
                             census_key))
      } else {
        
        
        variables <- ifelse(product=="pep" & subproduct == "charagegroups" & year == "2019", 
                            paste0(c( paste(str_replace(variables, "GEONAME", "NAME"))), collapse = ","), 
                            ifelse(product=="pep" ,
                                   paste0(c( paste(variables)), collapse = ","), ### verify if NAME is needed 
                                   paste0(c("NAME", "COUNTY", paste(variables)), collapse=",")))
        url <- ifelse(product == "pep" & municipio, 
                      paste0(url_base, "?get=",variables,"&for=county:*&in=state:72&key=", 
                             census_key), ifelse(product == "pep",
                                                 paste0(url_base, "?get=",variables,"&for=state:72&key=", census_key),
                                                 paste0(url_base, "?get=",variables,"&for=county:*&in=state:72&key=", 
                                                        census_key)))
        
        
        data_census <- GET(url)
      }
      
      if (product=="pep" & subproduct == "charagegroups" & year == "2019") {
        data_census <- fromJSON(rawToChar(data_census$content), flatten=T) %>% 
          as.data.frame() %>%
          row_to_names(1) %>%
          rename(GEONAME = "NAME")
      } else {
        data_census <- fromJSON(rawToChar(data_census$content), flatten=T) %>% 
          as.data.frame() %>%
          row_to_names(1)
      }
      
      variables_url <- paste0(url_base, "/variables")
      variables_information <- GET(variables_url)
      variables_information <- fromJSON(rawToChar(variables_information$content)) %>% 
        row_to_names(1) %>%
        as.data.frame() %>%
        filter(name %in% colnames(data_census))
    }
  }
  
  lista_censo <- list(data_census, variables_information)
  return(lista_censo)
}


################################################################################


make_tidy_pop_estimates <- function(year, municipio = F, 
                             group = F, table_type = NULL, census_key) {
  
  library(openxlsx)
  library(purrr)
  
  if (municipio) {
    variables = c("AGEGROUP", "SEX", "GEONAME", "POP")
    subproduct = "charagegroups"
  } else {
    variables = c("AGE", "SEX", "POP")
    subproduct = "charage"
  }
  
  
  if(any(year<2000)){
    years <- year[year<2000]
    start_year <- min(years)
    end_year <- max(years)
    
    temp_un <- get_un_data(start_year, end_year) %>%
      filter( sex != "Both sexes") %>%
      mutate(year = as.numeric(timeLabel)) %>%
      mutate(age = as.numeric(ageStart)) %>%
      mutate(sex = recode(sex, `Female`="F", `Male`="M")) %>%
      select(age, sex, value, year) %>%
      setNames(c("age","gender","estimate","year"))
  } else {temp_un <- NULL}
  
  if  (any(year <= 2014)) {
    year_api <- year[year<=2014]
    temp_x <- map_df(year_api, function(y) {
      tmp <- get_census_data(
        product = NULL,
        subproduct = NULL,
        year = y,
        variables = NULL,
        municipio = municipio,
        group = group,
        table_type = table_type,
        census_key = census_key
      )
      tmp <- tmp[[1]]
      return(tmp)
    })
  } 
  
  if (any(year < 2020) & any(year > 2014)) {
    year_api <- year[!(year %in% c(2020, 2021)) & year > 2014]
    temp <- map_df(year_api, function(y) {
      tmp <- get_census_data(
        product = "pep",
        subproduct = subproduct,
        year = y,
        variables = variables,
        municipio = municipio,
        group = group,
        table_type = table_type,
        census_key = census_key
      )
      tmp <- tmp[[1]]
      tmp$year <- as.character(y)
      return(tmp)
    })
  }
  
  
  if (all(year >= 2020) & municipio == TRUE){
    popest_muni_url <-
      "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-72.csv"
    
    out <- read.csv(popest_muni_url,
                    fileEncoding = "ISO-8859-1") %>%
      select(NAME, YEAR, matches("AGE.+_[FEM|MALE]")) %>%
      rename(municipio = "NAME", year = "YEAR") %>%
      filter(year != 1) %>%
      mutate(year = recode(as.character(year),  `2` = "2020", `3` = "2021")) %>%
      mutate(municipio = str_remove(municipio, " Municipio")) %>%
      pivot_longer(cols = c(-municipio,-year)) %>%
      mutate(name = str_remove(name, "AGE")) %>%
      separate(name, c("age", "gender")) %>%
      mutate(age = recode(
        age,
        `04` = "0004",
        `59` = "0509",
        `513` = "0513",
        `85PLUS` = "85Inf"
      )) %>%
      pivot_wider(names_from = age, values_from = value) %>%
      select(
        municipio,
        year,
        gender,
        "0004",
        "0509",
        "1014",
        "1519",
        "2024",
        "2529",
        "3034",
        "3539",
        "4044",
        "4549",
        "5054",
        "5559",
        "6064",
        "6569",
        "7074",
        "7579",
        "8084",
        "85Inf"
      ) %>%
      filter(!(gender %in% c("TOT"))) %>%
      mutate(gender = recode(gender, MALE = "M", FEM = "F")) %>%
      pivot_longer(-c("municipio", "gender", "year"),
                   names_to = "age",
                   values_to = "poblacion") %>%
      mutate(start = as.numeric(str_extract(age, "\\d{2}")),
             end = as.numeric(str_remove(age, "\\d{2}"))) %>%
      select(municipio, start, end, gender, poblacion, year)
  } else if (all(year>=2020) & municipio ==FALSE){
    
    popest_sya_url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2021/puerto-rico/asrh/prc-est2021-syasex.xlsx"
    
    out <- read.xlsx(popest_sya_url,
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
  } else if (any(year %in% c(2020, 2021)) &
             any(year < 2020) &
             municipio==TRUE) {
    popest_muni_url <-
      "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-agesex-72.csv"
    
    popest_muni_tidy <- read.csv(popest_muni_url,
                                 fileEncoding = "ISO-8859-1") %>%
      select(NAME, YEAR, matches("AGE.+_[FEM|MALE]")) %>%
      rename(municipio = "NAME", year = "YEAR") %>%
      filter(year != 1) %>%
      mutate(year = recode(as.character(year),  `2` = "2020", `3` = "2021")) %>%
      mutate(municipio = str_remove(municipio, " Municipio")) %>%
      pivot_longer(cols = c(-municipio,-year)) %>%
      mutate(name = str_remove(name, "AGE")) %>%
      separate(name, c("age", "gender")) %>%
      mutate(age = recode(
        age,
        `04` = "0004",
        `59` = "0509",
        `513` = "0513",
        `85PLUS` = "85Inf"
      )) %>%
      pivot_wider(names_from = age, values_from = value) %>%
      select(
        municipio,
        year,
        gender,
        "0004",
        "0509",
        "1014",
        "1519",
        "2024",
        "2529",
        "3034",
        "3539",
        "4044",
        "4549",
        "5054",
        "5559",
        "6064",
        "6569",
        "7074",
        "7579",
        "8084",
        "85Inf"
      ) %>%
      filter(!(gender %in% c("TOT"))) %>%
      mutate(gender = recode(gender, MALE = "M", FEM = "F")) %>%
      pivot_longer(-c("municipio", "gender", "year"),
                   names_to = "age",
                   values_to = "poblacion") %>%
      mutate(start = as.numeric(str_extract(age, "\\d{2}")),
             end = as.numeric(str_remove(age, "\\d{2}"))) %>%
      select(municipio, start, end, gender, poblacion, year)
    
    out <- temp %>%
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
    
  } else if (any(year %in% c(2020, 2021)) &
             any(year < 2020) &
             municipio==FALSE) {
    
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
    
    out <- temp %>% 
      select(AGE, SEX, POP, year) %>%
      setNames(c("age", "gender", "estimate", "year")) %>%
      filter(gender %in% c("1", "2"), !(age %in% c("999"))) %>%
      mutate(gender = recode(gender, `1` = "M", `2` = "F")) %>%
      mutate(age = as.numeric(age), 
             estimate = as.numeric(estimate)) %>%
      full_join(popest_sya_tidy, by = c("age", "gender", "estimate", "year"))
    
  } else if (!any(year %in% c(2020, 2021)) &
             municipio == TRUE) {
    out <- temp %>%
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
      mutate(poblacion = as.numeric(poblacion))
  } else if (!any(year %in% c(2020, 2021)) &
             municipio==FALSE &
             any(year>=2000)){
    out <- temp %>%
      select(AGE, SEX, POP, year) %>%
      setNames(c("age", "gender", "estimate", "year")) %>%
      filter(gender %in% c("1", "2"), !(age %in% c("999"))) %>%
      mutate(gender = recode(gender, `1` = "M", `2` = "F")) %>%
      mutate(age = as.numeric(age),
             estimate = as.numeric(estimate))
    
  } else {out <- NULL}
  
  out <- rbind(temp_un,temp_x, out)
  return(out)
}






  