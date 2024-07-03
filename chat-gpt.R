### chatgpt suggestion 
library(httr)
library(jsonlite)
library(data.table)
library(stringr)

get_un_data <- function(start_year, end_year){
  
  base_url_UNPD <- "https://population.un.org/dataportalapi/api/v1"
  
  puerto_rico_iso3 <- "PRI"
  puerto_rico_iso2 <- "PR"
  puerto_rico_id <- "630"
  indicator_code <- "47"
  start_year <- 2025 # start_year
  end_year <- 2026 #end_year
  
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

fetch_census_data <- function(url, variables) {
  response <- GET(url)
  if (http_type(response) != "application/json") {
    stop("Failed to fetch data from the Census API.")
  }
  data <- fromJSON(rawToChar(response$content), flatten = TRUE)
  data <- data.table::data.table(data)
  data.table::setnames(data, as.character(unlist(data[1, ])))
  data <- data[-1, ]
  return(data)
}

process_early_years <- function(year_new, census_key) {
  data_2000_2009 <- NULL
  data_2010_2014 <- NULL
  
  if (any(year_new %in% 2000:2009)) {
    url_2000_2009 <- paste0("http://api.census.gov/data/2000/pep/int_charage?get=POP,SEX,AGE,DATE_&for=state:72&key=", census_key)
    data_2000_2009 <- fetch_census_data(url_2000_2009)
    data_2000_2009 <- data_2000_2009[!(AGE == 999 | DATE_ %in% c("1", "12") | SEX == 0)]
    data_2000_2009[, date := as.numeric(DATE_)]
    data_2000_2009[, year := date + 1998] 
    data_2000_2009 <- data_2000_2009[, .(age = as.numeric(AGE), gender = ifelse(SEX == 1, "M", "F"), estimate = as.numeric(POP), year)]
    data_2000_2009 <- data_2000_2009[year %in% year_new]
  }
  
  if (any(year_new %in% 2010:2014)) {
    url_2010_2014 <- paste0("https://api.census.gov/data/2014/pep/prcagesex?get=POP,SEX,STNAME,AGE,DATE_&for=state:72&key=", census_key)
    data_2010_2014 <- fetch_census_data(url_2010_2014)
    data_2010_2014 <- data_2010_2014[!(AGE == 999 | DATE_ %in% c("1", "2") | SEX == 0)]
    data_2010_2014[, date := as.numeric(DATE_)]
    data_2010_2014[, year := date + 2007]
    data_2010_2014 <- data_2010_2014[, .(age = as.numeric(AGE), gender = ifelse(SEX == 1, "M", "F"), estimate = as.numeric(POP), year)]
    data_2010_2014 <- data_2010_2014[year %in% year_new]
  }
  
  return(rbind(data_2000_2009, data_2010_2014))
}

get_census_data <- function(product, subproduct, year, variables, 
                            municipio = FALSE, group = FALSE, 
                            table_type = NULL, census_key) {
  data_census <- NULL
  variables_information <- NULL
  if (!municipio) {
    if (any(year <= 2014)) {
    year_new <- year[year <= 2014]
    data_census <- process_early_years(year_new, census_key)
    } else  {
    year <- year[year > 2014 & year <= 2019]
    for (yr in year) {
      if (yr == 2018 & product == "pep") {
        url <- paste0("https://api.census.gov/data/2018/pep/charage?get=AGE,SEX,POP&DATE_CODE=11&for=state:72&key=", census_key)
        data_census <- fetch_census_data(url)
        data_census <- data_census[, DATE_CODE := NULL]
        } else {
        url_base <- ifelse(is.null(table_type), 
                           paste0("http://api.census.gov/data/", yr, "/", product, "/", subproduct), 
                           paste0("http://api.census.gov/data/", yr, "/", product, "/", subproduct, "/", table_type))
        group = !T
        if (group) {
          url <- ifelse(product == "pep", 
                        paste0(url_base, "?get=", variables, "&for=state:72&key=", census_key),
                        paste0(url_base, "?get=group(", variables,")&for=county:*&in=state:72&key=", census_key))
        } else {
          variables_query <- ifelse(product == "pep" & subproduct == "charagegroups" & yr == 2019,
                                    paste0(str_replace(variables, "GEONAME", "NAME"), collapse = ","),
                                    ifelse(product == "pep", paste0(variables, collapse = ","),
                                           paste0(c("NAME", "COUNTY", variables), collapse = ",")))
          url <- ifelse(product == "pep",
                               paste0(url_base, "?get=", variables_query, "&for=state:72&key=", census_key),
                               paste0(url_base, "?get=", variables_query, "&for=county:*&in=state:72&key=", census_key))
        }
        data_census <- fetch_census_data(url)
        }
      }
    }
    return(list(data_census))
  }
  
   else {
     data_muni <- NULL
     data_muni <- NULL
     data_muni <- NULL
  if (any(year >= 2000 & year < 2010)){
   
    year_data_muni <- year[year<=2010]
    
    data_muni_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/puerto-rico/prm-est00int-agesex-5yr.csv"
    
    data_muni <- data.table::fread(data_muni_url, encoding = "Latin-1")
    data_muni <- data_muni[,-c("SUMLEV", "MUNICIPIO", "ESTIMATESBASE2000", "CENSUS2010POP")]
    data.table::setnames(data_muni, c("municipio", "gender", "agegroup", names(data_muni)[-c(1:3)]))
    data_muni <- melt(data_muni, id.vars = c("municipio", "gender", "agegroup"),
                      variable.name = "year", value.name = "poblacion", verbose = FALSE)
    
    data_muni[, agegroup := as.character(agegroup)]
    data_muni <- data_muni[agegroup!="0"]
    data_muni[, agegroup := fcase(
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
    data_muni[,  `:=` (year=as.numeric(gsub("POPESTIMATE", "", year)), 
                       municipio = gsub(" Municipio", "", municipio), 
                       gender = fifelse(gender==1, "M", "F"), 
                       start = as.numeric(substr(agegroup, 1, 2)), 
                       end = as.numeric(sub("\\d{2}", "", agegroup)))]
    data_muni <- data_muni[,-"agegroup"][year %in% year_data_muni]
  }
  
  if (any(year >= 2010 & year < 2020)){
   
    year_data_muni <- year[year>2010 & year < 2020]
    
    data_muni_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/municipios/asrh/PRM-EST2020-AGESEX.csv"
    ## https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/prm-est2020-agesex.pdf##
    data_muni <- data.table::fread(data_muni_url, encoding = "Latin-1")
    data_muni <- data_muni[, -c("SUMLEV", "MUNICIPIO")]
    # Select relevant columns and rename them
    
    data.table::setnames(data_muni, c("municipio", "year", names(data_muni)[-c(1:2)]))
    data_muni <- data_muni[!(year %in% c(1,2,3,13,14))]
    data_muni[, year := as.numeric(year) + 2007]
    
    data_muni <- melt(data_muni, id.vars = c("municipio", "year"), 
                      variable.name = "name", value.name = "poblacion", verbose = FALSE)
    data_muni[, municipio := gsub(" Municipio", "", municipio)]
    
    # Split name column into age and gender columns
    data_muni <- data_muni[stringr::str_starts(name, "AGE")]
    data_muni <- data_muni[, c("age", "gender") := data.table::tstrsplit(name, "_")]
    data_muni[, age := gsub("AGE", "", age)]
    # Recode age values
    data_muni[, age := ifelse(age == "04", "0004",
                              ifelse(age == "59", "0509",
                                     ifelse(age == "513", "0513",
                                            ifelse(age == "85PLUS", "85Inf", age))))
    ]
    
    data_muni <- data_muni[!(age %in% c("16PLUS", "18PLUS", "65PLUS"))]
    data_muni <- data_muni[!(gender %in% c("TOT"))]
    
    # Recode gender values
    data_muni[gender == "MALE", gender := "M"]
    data_muni[gender == "FEM", gender := "F"]
    
    # Pivot longer to rearrange columns
    data_muni[, start := as.numeric(substr(age, 1, 2))]
    data_muni[, end := as.numeric(sub("\\d{2}", "", age))]
    data_muni[, dif := end - start]
    data_muni <- data_muni[dif %in% c(4, Inf)]
    data_muni <- data_muni[,-c("name", "age", "dif")]
    data_muni <- data_muni[year %in% year_data_muni]
  }
  
  if (any(year >= 2020 & year < 2024)) {
    year_pop_est <- year[year>=2020]
    
    popest_muni_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/asrh/cc-est2022-agesex-72.csv"
    
    ## Information about dataset ##
    ### https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2020-2022/cc-est2022-agesex.pdf##
    
    data_muni <- data.table::fread(popest_muni_url, encoding = "Latin-1")
    data_muni <- data_muni[, -c("SUMLEV", "MUNICIPIO")]
    # Select relevant columns and rename them
    
    data.table::setnames(data_muni, c("municipio", "year", names(data_muni)[-c(1:2)]))
    data_muni <- data_muni[year != 1]
    data_muni[, year := ifelse(year == 2, "2020", ifelse(year == 3, "2021", ifelse(year == 4, "2022", year)))]
    data_muni[, municipio := gsub(" Municipio", "", municipio)]
    
    # Pivot longer to gather age and gender columns
    data_muni <- data.table::melt(data_muni, id.vars = c("municipio", "year"),
                                         variable.name = "name", value.name = "poblacion", verbose = FALSE)
    
    # Split name column into age and gender columns
    data_muni <- data_muni[ str_starts(name, "AGE")]
    data_muni <- data_muni[, c("age", "gender") := data.table::tstrsplit(name, "_")]
    data_muni[, age := gsub("AGE", "", age)]
    # Recode age values
    data_muni[, age := ifelse(age == "04", "0004",
                                     ifelse(age == "59", "0509",
                                            ifelse(age == "513", "0513",
                                                   ifelse(age == "85PLUS", "85Inf", age))))
    ]
    
    data_muni <- data_muni[!(age %in% c("16PLUS", "18PLUS", "65PLUS"))]
    data_muni <- data_muni[!(gender %in% c("TOT"))]
    # Recode gender values
    data_muni[gender == "MALE", gender := "M"]
    data_muni[gender == "FEM", gender := "F"]
    
    # Pivot longer to rearrange columns
    data_muni[, start := as.numeric(substr(age, 1, 2))]
    data_muni[, end := as.numeric(sub("\\d{2}", "", age))]
    data_muni[, dif := end - start]
    data_muni <- data_muni[dif %in% c(4, Inf)]
    data_muni <- data_muni[,-c("name", "age", "dif")]
    data_muni <- data_muni[year %in% year_pop_est]
    }
  return(list(data_muni))
  }
}


process_population_data <- function(year, municipio, temp, temp_x, temp_un) {
  
  if (municipio) {
    out <- rbind(temp, temp_x)
  } else if (all(year >= 2020 & year <= 2022) & !municipio) {
    popest_sya_url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2022/puerto-rico/asrh/prc-est2022-syasex.xlsx"
    out <- read_sya_data(popest_sya_url)
  } else if ((any(year %in% c(2020:2022)) & any(year < 2020) & !municipio) | 
             any(year < 2020) & !municipio) {
    out <- merge_sya_data(temp, temp_x, year)
  } else {
    print("no option available")
  }
  return(out)
}

read_sya_data <- function(url) {
  data <- data.table(openxlsx::read.xlsx(url, fillMergedCells = TRUE, startRow = 5, check.names = TRUE, rows = c(1:93)))[, -c(2, 3, 4)]
  data.table::setnames(data, "X1", "age")
  data.table::setnames(data, colnames(data)[-1], gsub("\\.3", "_2022", colnames(data)[-1]))
  data.table::setnames(data, colnames(data)[-1], gsub("\\.2", "_2021", colnames(data)[-1]))
  data.table::setnames(data, colnames(data)[-1], gsub("\\.1", "_2020", colnames(data)[-1]))
  cols <- data[, grep("Total.", names(.SD), value = TRUE)]
  data <- data[, -..cols][age != "Total" & !is.na(age), ][, age := gsub("\\.|\\+", "", age)]
  data <- data.table::melt(data, id.vars = "age", variable.name = "gender_year", value.name = "estimate", verbose = FALSE)
  data <- data[, c("gender", "year") := data.table::tstrsplit(gender_year, "_", fixed = TRUE)][, -"gender_year"]
  data[gender == "Male", gender := "M"]
  data[gender == "Female", gender := "F"]
  data[, `:=` (estimate = as.numeric(estimate), age = as.numeric(age))]
  
  return(data)
}

merge_sya_data <- function(temp, temp_x, year) {
  popest_sya_url <- "https://www2.census.gov/programs-surveys/popest/tables/2020-2022/puerto-rico/asrh/prc-est2022-syasex.xlsx"
  popest_sya_tidy <- read_sya_data(popest_sya_url)
  year_pop_est <- year[year >= 2020]
  temp <- temp[, state := NULL]
  setnames(temp, c("age", "gender", "estimate", "year"))
  out <- temp
  out <- out[gender %in% c("1", "2") & !(age %in% "999")]
  out[, `:=` (gender = fifelse(gender == "1", "M", "F"), age = as.numeric(age), estimate = as.numeric(estimate))]
  out <- rbind(out, temp_x)
  out <- merge(out, popest_sya_tidy[year %in% year_pop_est], by = c("age", "gender", "estimate", "year"), all = TRUE)
  return(out)
}


make_tidy_pop_estimates <- function(year, municipio = FALSE, un_data = FALSE, census_key) {

  
  if (municipio & any(year < 2000)) {
    stop("Municipio level data is not available before 2000. Please change your request.")
  }
  
  group <- FALSE
  table_type <- NULL
  
  suppressWarnings({
    variables <- if (municipio) c("AGEGROUP", "SEX", "GEONAME", "POP") else c("AGE", "SEX", "POP")
    subproduct <- if (municipio) "charagegroups" else "charage"
    
    if (un_data) {
      print("un data out of service")
      # start_year <- min(year)
      # end_year <- max(year)
      # un_year <- year
      # temp_un <- data.table::data.table(get_un_data(start_year, end_year))
      # temp_un <- temp_un[sex != "Both sexes" & variant == "Median"]
      # temp_un <- temp_un[, year := as.numeric(timeLabel)][, age := as.numeric(ageStart)][, gender := data.table::fcase(
      #   sex == "Female", "F",
      #   sex == "Male", "M"
      # )][, estimate := as.numeric(value)]
      # temp_un <- temp_un[, .(age, gender, estimate, year)]
      # 
      # out <- temp_un[year %in% un_year]
      } else {
      temp_un <- NULL
      temp_x <- NULL
      temp <- NULL
      
      if (any(year < 2000)) {
        years <- year[year < 2000]
        start_year <- min(years)
        end_year <- max(years)
        
        temp_un <- data.table::data.table(get_un_data(start_year, end_year))
        temp_un <- temp_un[sex != "Both sexes" & variant == "Median"]
        temp_un <- temp_un[, year := as.numeric(timeLabel)][, age := as.numeric(ageStart)][, gender := data.table::fcase(
          sex == "Female", "F",
          sex == "Male", "M"
        )][, estimate := as.numeric(value)]
        temp_un <- temp_un[, .(age, gender, estimate, year)][year %in% years]
      }
      
      if (any(year <= 2014)) {
        year_api <- year[year <= 2014]
        temp_x <- lapply(year_api, function(y) {
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
        temp_x <- do.call(rbind, temp_x)
      }
      
      if (any( year > 2014)) {
        if (municipio) {
          year_api <- year
        } else {
        year_api <- year[!(year %in% c(2020:2022)) & year > 2014]
        }
        temp <- lapply(year_api, function(y) {
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
        temp <- do.call(rbind, temp)
        }
      
      out <- process_population_data(year, municipio, temp, temp_x, temp_un)
    }
    
    return(out)
  })
}



