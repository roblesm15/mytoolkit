### changing to data table ###

library(httr)
library(jsonlite)
library(janitor)
library(tidyverse)
library(purrr)
library(data.table)

wrangle_prcagesex <- function(data, b4_2010 = T) {
  if (b4_2010) {
    year_base <- 1998
  } else { 
    year_base <- 2007}
  data_census <- fromJSON(rawToChar(data$content), flatten=T) 
  data_census <- data.table::data.table(data_census)
  data.table::setnames(data_census, as.character(unlist(data_census[1,])))
  data_census <- data_census[-1,]
  data_census <- data_census[!(AGE == 999 | DATE_ %in% c("1", "2") | SEX == 0)]
  data_census[, date := as.numeric(DATE_)]
  data_census[, year := date + year_base] 
  data_census <- data_census[, .(age = AGE, gender = SEX, estimate = POP, year)]
  data_census[, gender := ifelse(gender == 1, "M", ifelse(gender == 2, "F", gender))]
  data_census <- data_census[year %in% year_new]
  data_census[, c("age", "estimate") := lapply(.SD, as.numeric), .SDcols = c("age", "estimate")]
  return(data_census)
}


get_un_data <- function(start_year, end_year){
  
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

  
  if (any(year <= 2014) & !municipio){
    year_new <- year[year <= 2014]
    
    if (any(year_new %in% c(2000:2009))){
      x <- GET(paste0("http://api.census.gov/data/2000/pep/int_charage?get=POP,SEX,AGE,DATE_&for=state:72&key=", census_key))
      data_census_x <- wrangle_prcagesex(x, b4_2010 = T)
      rm(x)
    } else {data_census_x = NULL}
    
    if (any(year_new %in% c(2010:2014))){
      y <- GET(paste0("https://api.census.gov/data/2014/pep/prcagesex?get=POP,SEX,STNAME,AGE,DATE_&for=state:72&key=", census_key))
      data_census_y <- wrangle_prcagesex(x, b4_2010 = F)
      rm(y)
    } else {data_census_y = NULL}
    data_census <- list(rbind(data_census_x, data_census_y))
    variables_information = NULL
  }
  
  if (any(year > 2014)){
    year = year[year > 2014]
    
    if (year == 2018 & product == "pep" & municipio == FALSE){
      url <- paste0("https://api.census.gov/data/2018/pep/charage?get=AGE,SEX,POP&DATE_CODE=11&for=state:72&key=",
                    census_key)
      data_census <- GET(url)
      data_census <- fromJSON(rawToChar(data_census$content), flatten=T) 
      data_census <- data.table::data.table(data_census)
      data.table::setnames(data_census, as.character(unlist(data_census[1,])))
      data_census <- data_census[-1,]
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
        data_census <- fromJSON(rawToChar(data_census$content), flatten=T) 
        data_census <- data.table::data.table(data_census)
        data.table::setnames(data_census, as.character(unlist(data_census[1,])))
        data_census <- data_census[-1,]
        data.table::setnames(data_census, "NAME", "GEONAME")
      } else {
        data_census <- fromJSON(rawToChar(data_census$content), flatten=T) 
        data_census <- data.table::data.table(data_census)
        data.table::setnames(data_census, as.character(unlist(data_census[1,])))
        data_census <- data_census[-1,]
      }
      
      variables_url <- paste0(url_base, "/variables")
      variables_information <- GET(variables_url)
      variables_information <- fromJSON(rawToChar(variables_information$content)) 
      variables_information <- data.table::data.table(variables_information)
      data.table::setnames(variables_information, as.character(unlist(variables_information[1,])))
      variables_information <- variables_information[-1,]
    }
  }
  
  lista_censo <- list(data_census, variables_information)
  return(lista_censo)
}


################################################################################



make_tidy_pop_estimates <- function(year, municipio = FALSE, 
                                    un_data = FALSE, census_key) {

  
  if (municipio & any(year < 2000)) {
    stop("Municipio level data is not available before 2000. Please change your request.")
  }
  
  group = FALSE 
  table_type = NULL
  
  suppressWarnings({if (municipio) {
    variables = c("AGEGROUP", "SEX", "GEONAME", "POP")
    subproduct = "charagegroups"
  } else {
    variables = c("AGE", "SEX", "POP")
    subproduct = "charage"
  }
  
  ### Downloads data from API: some parameter combinations will require download
  ### from other links
    
  if (un_data) {
    start_year <- min(year)
    end_year <- max(year)
    un_year <- year
    temp_un <- data.table::data.table(get_un_data(start_year, end_year))
    temp_un <- temp_un[sex != "Both sexes" & variant == "Median"]
    temp_un <- temp_un[
      , year := as.numeric(timeLabel)][
        , age := as.numeric(ageStart)][
          , gender := data.table::fcase(
            sex == "Female", "F",
            sex == "Male", "M"
          )][, estimate := as.numeric(value)]
    temp_un <- temp_un[, c("age", "gender", "estimate", "year")]
    
    out <- temp_un[year %in% un_year]
    rm(un_year)
  } else {
    
    if(any(year<2000)){
      years <- year[year<2000]
      start_year <- min(years)
      end_year <- max(years)
      
      temp_un <- data.table::data.table(get_un_data(start_year, end_year))
      temp_un <- temp_un[sex != "Both sexes" & variant == "Median"]
      temp_un <- temp_un[
        , year := as.numeric(timeLabel)][
          , age := as.numeric(ageStart)][
            , gender := data.table::fcase(
              sex == "Female", "F",
              sex == "Male", "M"
            )][, estimate := as.numeric(value)]
      temp_un <- temp_un[, c("age", "gender", "estimate", "year")][year %in% years]
      
    } else {temp_un <- NULL}
    
    if  (any(year <= 2014) & municipio == FALSE) {
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
    } else {temp_x <- NULL} 
    
    if (any(year < 2020 & year > 2014)) {
      year_api <- year[!(year %in% c(2020: 2022)) & year > 2014]
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
    
    
    if (all(year >= 2020 & year <= 2022) & municipio){
      
      popest_muni_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/asrh/cc-est2022-agesex-72.csv"
      
      ## Information about dataset ##
      ### https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2022/cc-est2022-agesex.pdf##
      
      out <- data.table::fread(popest_muni_url, encoding = "Latin-1")
      out <- out[, -c("SUMLEV", "MUNICIPIO")]
      # Select relevant columns and rename them
     
      data.table::setnames(out, c("municipio", "year", names(out)[-c(1:2)]))
      out <- out[year != 1]
      out[, year := ifelse(year == 2, "2020", ifelse(year == 3, "2021", ifelse(year == 4, "2022", year)))]
      out[, municipio := gsub(" Municipio", "", municipio)]
      
      # Pivot longer to gather age and gender columns
      out <- data.table::melt(out, id.vars = c("municipio", "year"),
                                variable.name = "name", value.name = "poblacion", verbose = FALSE)
      
      # Split name column into age and gender columns
      out <- out[ str_starts(name, "AGE")]
      out <- out[, c("age", "gender") := data.table::tstrsplit(name, "_")]
      out[, age := gsub("AGE", "", age)]
      # Recode age values
      out[, age := ifelse(age == "04", "0004",
                          ifelse(age == "59", "0509",
                                 ifelse(age == "513", "0513",
                                        ifelse(age == "85PLUS", "85Inf", age))))
      ]
      
      out <- out[!(age %in% c("16PLUS", "18PLUS", "65PLUS"))]
      out <- out[!(gender %in% c("TOT"))]
      
      # Recode gender values
      out[gender == "MALE", gender := "M"]
      out[gender == "FEM", gender := "F"]
      
      # Pivot longer to rearrange columns
      out[, start := as.numeric(substr(age, 1, 2))]
      out[, end := as.numeric(sub("\\d{2}", "", age))]
      out[, dif := end - start]
      out <- out[dif %in% c(4, Inf)]
      out <- out[,-c("name", "age", "dif")]
    
    } else if (all(year>=2020 & year<=2022) & municipio == FALSE){
      
      popest_sya_url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2022/puerto-rico/asrh/prc-est2022-syasex.xlsx"
      
      out <- data.table(openxlsx::read.xlsx(popest_sya_url,
                                                        fillMergedCells = T, startRow = 5, check.names = T,
                                                        rows = c(1:93)))[,-c(2,3,4)]
      setnames(out, "X1", "age")
      setnames(out, colnames(out)[-1], gsub("\\.3", "_2022", colnames(out)[-1]))
      setnames(out, colnames(out)[-1], gsub("\\.2", "_2021", colnames(out)[-1]))
      setnames(out, colnames(out)[-1], gsub("\\.1", "_2020", colnames(out)[-1]))
      
      cols <- out[,grep("Total.", names(.SD), value = TRUE)] 
      out <- out[,-..cols][age !="Total" & !is.na(age),][, age := gsub("\\.|\\+", "", age)]
      out <- melt(out, id.vars = "age", variable.name = "gender_year", value.name = "estimate",  verbose = FALSE)
      out <- out[, c("gender", "year") := tstrsplit(gender_year, "_", fixed = TRUE)][,-"gender_year"]
      out[gender == "Male", gender := "M"][gender == "Female", gender := "F"][, `:=` (estimate = as.numeric(estimate), age = as.numeric(age))]
      
    } else if (any(year %in% c(2020:2022)) &
               all(year >= 2014) &
               any(year < 2020) &
               municipio==TRUE) {
      
      year_pop_est <- year[year>=2020]
      
      popest_muni_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/asrh/cc-est2022-agesex-72.csv"
      
      ## Information about dataset ##
      ### https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2022/cc-est2022-agesex.pdf##
      
      popest_muni_tidy <- data.table::fread(popest_muni_url, encoding = "Latin-1")
      popest_muni_tidy <- popest_muni_tidy[, -c("SUMLEV", "MUNICIPIO")]
      # Select relevant columns and rename them
      
      data.table::setnames(popest_muni_tidy, c("municipio", "year", names(popest_muni_tidy)[-c(1:2)]))
      popest_muni_tidy <- popest_muni_tidy[year != 1]
      popest_muni_tidy[, year := ifelse(year == 2, "2020", ifelse(year == 3, "2021", ifelse(year == 4, "2022", year)))]
      popest_muni_tidy[, municipio := gsub(" Municipio", "", municipio)]
      
      # Pivot longer to gather age and gender columns
      popest_muni_tidy <- data.table::melt(popest_muni_tidy, id.vars = c("municipio", "year"),
                              variable.name = "name", value.name = "poblacion", verbose = FALSE)
      
      # Split name column into age and gender columns
      popest_muni_tidy <- popest_muni_tidy[ str_starts(name, "AGE")]
      popest_muni_tidy <- popest_muni_tidy[, c("age", "gender") := data.table::tstrsplit(name, "_")]
      popest_muni_tidy[, age := gsub("AGE", "", age)]
      # Recode age values
      popest_muni_tidy[, age := ifelse(age == "04", "0004",
                          ifelse(age == "59", "0509",
                                 ifelse(age == "513", "0513",
                                        ifelse(age == "85PLUS", "85Inf", age))))
      ]
      
      popest_muni_tidy <- popest_muni_tidy[!(age %in% c("16PLUS", "18PLUS", "65PLUS"))]
      popest_muni_tidy <- popest_muni_tidy[!(gender %in% c("TOT"))]
      
      # Recode gender values
      popest_muni_tidy[gender == "MALE", gender := "M"]
      popest_muni_tidy[gender == "FEM", gender := "F"]
      
      # Pivot longer to rearrange columns
      popest_muni_tidy[, start := as.numeric(substr(age, 1, 2))]
      popest_muni_tidy[, end := as.numeric(sub("\\d{2}", "", age))]
      popest_muni_tidy[, dif := end - start]
      popest_muni_tidy <- popest_muni_tidy[dif %in% c(4, Inf)]
      popest_muni_tidy <- popest_muni_tidy[,-c("name", "age", "dif")]
      popest_muni_tidy <- popest_muni_tidy[year %in% year_pop_est]
      rm(year_pop_est)
      
      ### Years  2014-2019 ###
      out <- copy(temp)
      out[, c("state", "county") := NULL]
      setnames(out, c("agegroup", "gender", "municipio", "poblacion", "year"))
      out[, agegroup := as.numeric(agegroup)]
      out <- out[agegroup %in% 1:18 & gender %in% c("1", "2")]
      out[gender == 1, gender := "M"]
      out[gender == 2, gender := "F"]
      out[, agegroup := as.character(agegroup)]
      out[, agegroup := fcase(
        agegroup == "1", "0004",
        agegroup == "2", "0509",
        agegroup == "3", "1014",
        agegroup == "4", "1519",
        agegroup == "5", "2024",
        agegroup == "6", "2529",
        agegroup == "7", "3034",
        agegroup == "8", "3539",
        agegroup == "9", "4044",
        agegroup == "10", "4549",
        agegroup == "11", "5054",
        agegroup == "12", "5559",
        agegroup == "13", "6064",
        agegroup == "14", "6569",
        agegroup == "15", "7074",
        agegroup == "16", "7579",
        agegroup == "17", "8084",
        agegroup == "18", "85Inf",
        TRUE, NA_character_
      )]
      out[, c("start", "end") :=tstrsplit(agegroup, "(?<=\\d{2})(?=\\d{2}|Inf)", perl = TRUE)]
      out[, municipio := gsub(" Municipio, Puerto Rico", "", municipio)]
      out[, agegroup := NULL]
      out[, poblacion := as.numeric(poblacion)]
      out[, start := as.numeric(start)]
      out[, end := as.numeric(end)]
      out <- merge(out, popest_muni_tidy, by = c("municipio", "gender", "year", "start", "end", "poblacion"), all = TRUE)
      
      
    } else if (any(year %in% c(2020:2022)) &
               any(year < 2014)  &
               municipio) {
      
      year_pop_est <- year[year>=2020]
      
      popest_muni_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/asrh/cc-est2022-agesex-72.csv"
      
      ## Information about dataset ##
      ### https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2022/cc-est2022-agesex.pdf##
      
      popest_muni_tidy <- data.table::fread(popest_muni_url, encoding = "Latin-1")
      popest_muni_tidy <- popest_muni_tidy[, -c("SUMLEV", "MUNICIPIO")]
      # Select relevant columns and rename them
      
      data.table::setnames(popest_muni_tidy, c("municipio", "year", names(popest_muni_tidy)[-c(1:2)]))
      popest_muni_tidy <- popest_muni_tidy[year != 1]
      popest_muni_tidy[, year := ifelse(year == 2, "2020", ifelse(year == 3, "2021", ifelse(year == 4, "2022", year)))]
      popest_muni_tidy[, municipio := gsub(" Municipio", "", municipio)]
      
      # Pivot longer to gather age and gender columns
      popest_muni_tidy <- data.table::melt(popest_muni_tidy, id.vars = c("municipio", "year"),
                                           variable.name = "name", value.name = "poblacion", verbose = FALSE)
      
      # Split name column into age and gender columns
      popest_muni_tidy <- popest_muni_tidy[ str_starts(name, "AGE")]
      popest_muni_tidy <- popest_muni_tidy[, c("age", "gender") := data.table::tstrsplit(name, "_")]
      popest_muni_tidy[, age := gsub("AGE", "", age)]
      # Recode age values
      popest_muni_tidy[, age := ifelse(age == "04", "0004",
                                       ifelse(age == "59", "0509",
                                              ifelse(age == "513", "0513",
                                                     ifelse(age == "85PLUS", "85Inf", age))))
      ]
      
      popest_muni_tidy <- popest_muni_tidy[!(age %in% c("16PLUS", "18PLUS", "65PLUS"))]
      popest_muni_tidy <- popest_muni_tidy[!(gender %in% c("TOT"))]
      
      # Recode gender values
      popest_muni_tidy[gender == "MALE", gender := "M"]
      popest_muni_tidy[gender == "FEM", gender := "F"]
      
      # Pivot longer to rearrange columns
      popest_muni_tidy[, start := as.numeric(substr(age, 1, 2))]
      popest_muni_tidy[, end := as.numeric(sub("\\d{2}", "", age))]
      popest_muni_tidy[, dif := end - start]
      popest_muni_tidy <- popest_muni_tidy[dif %in% c(4, Inf)]
      popest_muni_tidy <- popest_muni_tidy[,-c("name", "age", "dif")]
      popest_muni_tidy <- popest_muni_tidy[year %in% year_pop_est]
      rm(year_pop_est)
      
      if (any(year <= 2010 & year >= 2000)){
      ### Data before 2000-2010
      year_temp_2000 <- year[year<=2010]
      
      temp_2000_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/puerto-rico/prm-est00int-agesex-5yr.csv"
      
      temp_2000 <- data.table::fread(temp_2000_url, encoding = "Latin-1")
      temp_2000 <- temp_2000[,-c("SUMLEV", "MUNICIPIO", "ESTIMATESBASE2000", "CENSUS2010POP")]
      data.table::setnames(temp_2000, c("municipio", "gender", "agegroup", names(temp_2000)[-c(1:3)]))
      temp_2000 <- melt(temp_2000, id.vars = c("municipio", "gender", "agegroup"),
                        variable.name = "year", value.name = "poblacion", verbose = FALSE)
      
      temp_2000[, agegroup := as.character(agegroup)]
      temp_2000 <- temp_2000[agegroup!="0"]
      temp_2000[, agegroup := fcase(
        agegroup == "1", "0004",
        agegroup == "2", "0509",
        agegroup == "3", "1014",
        agegroup == "4", "1519",
        agegroup == "5", "2024",
        agegroup == "6", "2529",
        agegroup == "7", "3034",
        agegroup == "8", "3539",
        agegroup == "9", "4044",
        agegroup == "10", "4549",
        agegroup == "11", "5054",
        agegroup == "12", "5559",
        agegroup == "13", "6064",
        agegroup == "14", "6569",
        agegroup == "15", "7074",
        agegroup == "16", "7579",
        agegroup == "17", "8084",
        agegroup == "18", "85Inf",
        TRUE, NA_character_
      )]
      temp_2000[,  `:=` (year=as.numeric(gsub("POPESTIMATE", "", year)), 
                         municipio = gsub(" Municipio", "", municipio), 
                         gender = fifelse(gender==1, "M", "F"), 
                         start = as.numeric(substr(agegroup, 1, 2)), 
                         end = as.numeric(sub("\\d{2}", "", agegroup)))]
      temp_2000 <- temp_2000[,-"agegroup"][year %in% year_temp_2000]
      } else {temp_2000 = NULL}
      
      if (any(year > 2010 & year < 2020)){
        
        year_temp_2010 <- year[year>2010 & year < 2020]
        
        temp_2010_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/municipios/asrh/PRM-EST2020-AGESEX.csv"
        ## https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/prm-est2020-agesex.pdf##
        temp_2010 <- data.table::fread(temp_2010_url, encoding = "Latin-1")
        temp_2010 <- temp_2010[, -c("SUMLEV", "MUNICIPIO")]
        # Select relevant columns and rename them
        
        data.table::setnames(temp_2010, c("municipio", "year", names(temp_2010)[-c(1:2)]))
        temp_2010 <- temp_2010[!(year %in% c(1,2,3,13,14))]
        temp_2010[, year := fcase(
          year == 4, "2011",
          year == 5, "2012",
          year == 6, "2013",
          year == 7, "2014",
          year == 8, "2015",
          year == 9, "2016",
          year == 10, "2017",
          year == 11, "2018",
          year == 12, "2019")]
        
        temp_2010 <- melt(temp_2010, id.vars = c("municipio", "year"), 
                        variable.name = "name", value.name = "poblacion", verbose = FALSE)
        temp_2010[, municipio := gsub(" Municipio", "", municipio)]
         
        # Split name column into age and gender columns
        temp_2010 <- temp_2010[ str_starts(name, "AGE")]
        temp_2010 <- temp_2010[, c("age", "gender") := data.table::tstrsplit(name, "_")]
        temp_2010[, age := gsub("AGE", "", age)]
        # Recode age values
        temp_2010[, age := ifelse(age == "04", "0004",
                                         ifelse(age == "59", "0509",
                                                ifelse(age == "513", "0513",
                                                       ifelse(age == "85PLUS", "85Inf", age))))
        ]
        
        temp_2010 <- temp_2010[!(age %in% c("16PLUS", "18PLUS", "65PLUS"))]
        temp_2010 <- temp_2010[!(gender %in% c("TOT"))]
        
        # Recode gender values
        temp_2010[gender == "MALE", gender := "M"]
        temp_2010[gender == "FEM", gender := "F"]
        
        # Pivot longer to rearrange columns
        temp_2010[, start := as.numeric(substr(age, 1, 2))]
        temp_2010[, end := as.numeric(sub("\\d{2}", "", age))]
        temp_2010[, dif := end - start]
        temp_2010 <- temp_2010[dif %in% c(4, Inf)]
        temp_2010 <- temp_2010[,-c("name", "age", "dif")]
        temp_2010 <- temp_2010[year %in% year_temp_2010]
        } else {temp_2010=NULL}
      
      out <- rbind(temp_2000, temp_2010, popest_muni_tidy)
      
    
      } else if (any(year %in% c(2020:2022)) &
               any(year < 2020) &
               municipio==FALSE) {
      year_sya <- year[year %in% c(2020:2022)]
      popest_sya_url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2022/puerto-rico/asrh/prc-est2022-syasex.xlsx"
      
      
      popest_sya_tidy <- data.table(openxlsx::read.xlsx(popest_sya_url,
                                                        fillMergedCells = T, startRow = 5, check.names = T,
                                                        rows = c(1:93)))[,-c(2,3,4)]
      setnames(popest_sya_tidy, "X1", "age")
      setnames(popest_sya_tidy, colnames(popest_sya_tidy)[-1], gsub("\\.3", "_2022", colnames(popest_sya_tidy)[-1]))
      setnames(popest_sya_tidy, colnames(popest_sya_tidy)[-1], gsub("\\.2", "_2021", colnames(popest_sya_tidy)[-1]))
      setnames(popest_sya_tidy, colnames(popest_sya_tidy)[-1], gsub("\\.1", "_2020", colnames(popest_sya_tidy)[-1]))
      
      cols <- popest_sya_tidy[,grep("Total.", names(.SD), value = TRUE)] 
      popest_sya_tidy <- popest_sya_tidy[,-..cols][age !="Total" & !is.na(age),][, age := gsub("\\.|\\+", "", age)]
      popest_sya_tidy <- melt(popest_sya_tidy, id.vars = "age", variable.name = "gender_year", value.name = "estimate", verbose = FALSE)
      popest_sya_tidy <- popest_sya_tidy[, c("gender", "year") := tstrsplit(gender_year, "_", fixed = TRUE)][,-"gender_year"]
      popest_sya_tidy[gender == "Male", gender := "M"][gender == "Female", gender := "F"][, `:=` (estimate = as.numeric(estimate),age = as.numeric(age))]
      popest_sya_tidy <- popest_sya_tidy[year %in% year_sya]
      
      out <- temp[, c("AGE", "SEX", "POP", "year"), with = FALSE]
      setnames(out, c("age", "gender", "estimate", "year"))
      out <- out[gender %in% c("1", "2") & !(age %in% "999")]
      out[, `:=` (gender = fifelse(gender == "1", "M", "F"), age = as.numeric(age), estimate = as.numeric(estimate))]
      
      out <- merge(out, popest_sya_tidy, by = c("age", "gender", "estimate", "year"), all = TRUE)
      
      
    } else if (!any(year %in% c(2020:2022)) &
               municipio == TRUE) {
      
      if (any(year <= 2010 & year >= 2000)){
        ### Data before 2000-2010
        year_temp_2000 <- year[year<=2010]
        
        temp_2000_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/puerto-rico/prm-est00int-agesex-5yr.csv"
        
        temp_2000 <- data.table::fread(temp_2000_url, encoding = "Latin-1")
        temp_2000 <- temp_2000[,-c("SUMLEV", "MUNICIPIO", "ESTIMATESBASE2000", "CENSUS2010POP")]
        data.table::setnames(temp_2000, c("municipio", "gender", "agegroup", names(temp_2000)[-c(1:3)]))
        temp_2000 <- melt(temp_2000, id.vars = c("municipio", "gender", "agegroup"),
                          variable.name = "year", value.name = "poblacion", verbose = FALSE)
        
        temp_2000[, agegroup := as.character(agegroup)]
        temp_2000 <- temp_2000[agegroup!="0"]
        temp_2000[, agegroup := fcase(
          agegroup == "1", "0004",
          agegroup == "2", "0509",
          agegroup == "3", "1014",
          agegroup == "4", "1519",
          agegroup == "5", "2024",
          agegroup == "6", "2529",
          agegroup == "7", "3034",
          agegroup == "8", "3539",
          agegroup == "9", "4044",
          agegroup == "10", "4549",
          agegroup == "11", "5054",
          agegroup == "12", "5559",
          agegroup == "13", "6064",
          agegroup == "14", "6569",
          agegroup == "15", "7074",
          agegroup == "16", "7579",
          agegroup == "17", "8084",
          agegroup == "18", "85Inf",
          TRUE, NA_character_
        )]
        temp_2000[,  `:=` (year=as.numeric(gsub("POPESTIMATE", "", year)), 
                           municipio = gsub(" Municipio", "", municipio), 
                           gender = fifelse(gender==1, "M", "F"), 
                           start = as.numeric(substr(agegroup, 1, 2)), 
                           end = as.numeric(sub("\\d{2}", "", agegroup)))]
        temp_2000 <- temp_2000[,-"agegroup"][year %in% year_temp_2000]
      } else {temp_2000 = NULL}
      
      if (any(year > 2010 & year < 2020)){
        
        year_temp_2010 <- year[year>2010 & year < 2020]
        
        temp_2010_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/municipios/asrh/PRM-EST2020-AGESEX.csv"
        ## https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/prm-est2020-agesex.pdf##
        temp_2010 <- data.table::fread(temp_2010_url, encoding = "Latin-1")
        temp_2010 <- temp_2010[, -c("SUMLEV", "MUNICIPIO")]
        # Select relevant columns and rename them
        
        data.table::setnames(temp_2010, c("municipio", "year", names(temp_2010)[-c(1:2)]))
        temp_2010 <- temp_2010[!(year %in% c(1,2,3,13,14))]
        temp_2010[, year := fcase(
          year == 4, "2011",
          year == 5, "2012",
          year == 6, "2013",
          year == 7, "2014",
          year == 8, "2015",
          year == 9, "2016",
          year == 10, "2017",
          year == 11, "2018",
          year == 12, "2019")]
        
        temp_2010 <- melt(temp_2010, id.vars = c("municipio", "year"), 
                          variable.name = "name", value.name = "poblacion", verbose = FALSE)
        temp_2010[, municipio := gsub(" Municipio", "", municipio)]
        
        # Split name column into age and gender columns
        temp_2010 <- temp_2010[ str_starts(name, "AGE")]
        temp_2010 <- temp_2010[, c("age", "gender") := data.table::tstrsplit(name, "_")]
        temp_2010[, age := gsub("AGE", "", age)]
        # Recode age values
        temp_2010[, age := ifelse(age == "04", "0004",
                                  ifelse(age == "59", "0509",
                                         ifelse(age == "513", "0513",
                                                ifelse(age == "85PLUS", "85Inf", age))))
        ]
        
        temp_2010 <- temp_2010[!(age %in% c("16PLUS", "18PLUS", "65PLUS"))]
        temp_2010 <- temp_2010[!(gender %in% c("TOT"))]
        
        # Recode gender values
        temp_2010[gender == "MALE", gender := "M"]
        temp_2010[gender == "FEM", gender := "F"]
        
        # Pivot longer to rearrange columns
        temp_2010[, start := as.numeric(substr(age, 1, 2))]
        temp_2010[, end := as.numeric(sub("\\d{2}", "", age))]
        temp_2010[, dif := end - start]
        temp_2010 <- temp_2010[dif %in% c(4, Inf)]
        temp_2010 <- temp_2010[,-c("name", "age", "dif")]
        temp_2010 <- temp_2010[year %in% year_temp_2010]
      } else {temp_2010=NULL}
      
      out <- rbind(temp_2000, temp_2010)
      
    } else if (!any(year %in% c(2020, 2021)) &
               municipio==FALSE &
               any(year>=2015)){

      out <- data.table(temp)[,-c("state")]
      setnames(out, c("age", "gender", "estimate", "year"))
      out <- out[gender %in% c("1", "2") & !age %in% c("999")]
      out[, gender := fcase(
        gender == "1", "M",
        gender == "2", "F"
      )]
      out[, c("age", "estimate") := .(as.numeric(age), as.numeric(estimate))]

      } else {out <- NULL}
    out <- rbind(temp_un,temp_x, out)
  }
  return(out)
  
}
)
}
