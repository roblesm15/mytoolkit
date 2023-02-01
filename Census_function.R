#### Pulling Puerto Rico Census Data from APIs ####

## product <- acs1 ; acs3, acs5,  
### product details here: https://api.census.gov/data.html
## acs1 only provides estimates for counties where population >= 65,000
## acs5 https://www.census.gov/data/developers/data-sets/acs-5year.html




get_census_data <- function(product, subproduct, year, variables, municipio = F, group = F, 
                            table_type = NULL, census_key) {
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)
  
  
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
  }
  
  data_census <- GET(url)
     
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
  lista_censo <- list(data_census, variables_information)
  return(lista_censo)
}



  