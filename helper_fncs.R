# Helper functions #

library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)
library(stringr)
#age ranges


ageRange_starts <- c( seq(0, 85, 5))
ageRange_ends <- c(ageRange_starts[-1]-1, Inf)
ageRange_levels <- paste(ageRange_starts, ageRange_ends, sep = "-")
ageRange_levels[length(ageRange_levels)] <- paste0(ageRange_starts[length(ageRange_levels)],"Inf")


extract_numeric_ranges <- function(text) {

  # Extract numeric values using regular expression
  numeric_values <- paste(str_match(text, "(\\d+)\\D*(\\d*)")[, 2:3], collapse = "-")
  return(numeric_values)
  # Format into ranges with "-"
  # if (numeric_values[,2]=="") {
  #   return(numeric_values[,1])
  # } else {
  #   return(paste(numeric_values, collapse = "-"))
  # }
}

# Convert the named list to a data.table
vars_names <- function(list_var) {
  x <- rbindlist(lapply(names(list_var), function(name) {
    # Create a data.table for each list element, including the name
    dt_item <- as.data.table(list_var[[name]])
    dt_item[, name := name]
  }), fill = TRUE)
  x <- x[,c("name", "label")]
  return(x)
}



## Wrangle functions

wrangle_dat <- function(dat, product = NULL, variable_names = NULL, year.input = NULL) {
  
  if (product == "acs") {
    cols_end_with <- c("E", "M")
    pattern_var <- "B01001_"
    male_no <- c(3:25)
    female_no <- c(27:49)
  } else if (product == "decennial") {
    
    if (year.input == 2020) {
      male_no <- c(3:25)
      female_no <- c(27:49)
      cols_end_with <- "N"
      pattern_var <- "P12_0"
    } else if (year.input == 2010) {
      male_no <- c(3:25)
      female_no <- c(27:49)
      cols_end_with <- ""
      pattern_var <- "P0120"
    }  else  {
      male_no <- c(3:25)
      female_no <- c(27:49)
      cols_end_with <- ""
      pattern_var <- "P0120"
    }
  }
  
  dat <- as.data.table(dat)
  dat <- setNames(dat, as.character(dat[1,]))
  dat <- dat[-1,]
  dat[, municipio := NAME]
  dat[, NAME := NULL]
  dat[, GEO_ID := NULL]
  dat[, county := NULL]
  dat[, state := NULL]
  dat[, municipio := str_remove(municipio, " Municipio, Puerto Rico")]
  if (length(cols_end_with) == 2) {
    cols_to_keep <- c("municipio", 
                      colnames(dat)[endsWith(colnames(dat),cols_end_with[1])],
                      colnames(dat)[endsWith(colnames(dat),cols_end_with[2])])
  } else {
    cols_to_keep <- c("municipio", 
                      colnames(dat)[endsWith(colnames(dat),cols_end_with)])
  }
 
  dat <- dat[, ..cols_to_keep]
  dat <- melt(dat,
              id.vars = "municipio", 
              measure.vars = patterns(pattern_var),
              variable.name = "variable",
              value.name = "estimate")

  dat <- merge(dat, variable_names, by.x = "variable", by.y = "name", all.x = T)
  
  if (product == "acs") {
    dat[, label := ifelse(is.na(label), "MOE", label)]
    dat <- dat[, variable := str_remove(variable,paste0(cols_end_with, collapse= "|"))]
    moe_dat <- dat[label == "MOE"]
    moe_dat[, moe := estimate]
    est_dat <- dat[label != "MOE"]
    dat <- cbind(moe_dat, est_dat)
    dcast(dat, municipio + estimate ~ label, fun.aggregate=identity, value.var = "label", subset=.(label == "MOE"))
    
  }

  dat[, variable := str_remove(variable, pattern_var)]
  if (product == "acs") {
    dat <- dat[, type := str_extract(variable,paste0(cols_end_with, collapse= "|"))]
    dat <- dat[, variable := as.numeric(str_remove(variable, paste0(cols_end_with, collapse= "|")))]
  }
  if (all(cols_end_with != "") & product != "acs") {
    dat[, variable := as.numeric(str_remove(variable, cols_end_with))]
  } else {
    dat[, variable := as.numeric(variable)]
  }
  dat[, gender := NA]
  dat[, gender := fcase(variable %in% male_no, "M", 
                        variable %in% female_no, "F",
                        default = NA)]
  dat[, label := str_remove(label, "Estimate!!Total:!!\\b(?:Female|Male)\\b:!!")]
  dat <- dat[!is.na(gender)]
  dat[, agegroup := sapply(label, extract_numeric_ranges)]
  dat[, agegroup :=  fifelse(agegroup == "5-", "0-4",
                             fifelse(agegroup == "85-", "85+", agegroup))]
 
  if (product == "acs") {
    dat <- dat[, c("municipio", "agegroup", "gender", "estimate", "type")]
    dat[, moe := ifelse(type == "M", estimate, NA)]
    dat2 <- pivot_wider(dat, 
                        id_cols = c(municipio , agegroup, gender),
                        names_from =  type,
                        values_from =  estimate)

  } else {
    dat <- dat[, c("municipio", "agegroup", "gender", "estimate")]
    dat <- dat[, keyby = .(municipio, agegroup, gender), 
             .(estimate = sum(as.numeric(estimate)))]
  }
  return(dat)
  
}

wrangle_pep <- function(dat, municipio = NULL, year.input = year_input) {
  
  dat <- as.data.table(dat)
  if (year.input < 2020) {
    dat <- setNames(dat, as.character(dat[1,]))
    dat <- dat[-1,]
    dat[, year :=  sub(".*([0-9]{4}).*", "\\1", DATE_DESC)]
    dat <- dat[!grepl("census|base", DATE_DESC, ignore.case = TRUE)]
  
    if (!municipio) {
      dat <- dat[, c("SEX", "AGE", "year", "POP")]
      dat <- setNames(dat, c("gender", "age", "year", "estimate"))
      dat <- dat[gender!=0 & age != 999]
      dat[, gender := recode(gender, 
                             `1` = "M", 
                             `2` = "F")]
      dat <- dat[, age := as.numeric(age)]
      dat <- dat[year %in% year.input]
    } else {
      dat <- dat[, c("NAME","SEX", "AGEGROUP", "year", "POP")]
      dat[, municipio := str_remove(NAME, " Municipio, Puerto Rico")]
      dat[, NAME := NULL]
      dat <- setNames(dat, c("gender", "agegroup", "year", "estimate", "municipio"))
      ageRanges <- setNames(ageRange_levels, 1:18)
      dat[, ageRange := ageRanges[agegroup]]
      dat <- dat[gender!=0 & !is.na(ageRange)]
      dat[, gender := recode(gender, 
                             `1` = "M", 
                             `2` = "F")]
      dat[, agegroup := NULL]
      dat <- dat[year %in% year.input]
    } 
    
  } else if (year.input >= 2020) {
     dat[, municipio := str_remove(CTYNAME, " Municipio")] 
     dat[, year := YEAR + 2020 - 2]
     dat <- dat[year >= 2020]
     dat <- dat[year %in% year.input]
     dat <- dat[, c( "municipio", "AGE", "TOT_MALE", "TOT_FEMALE")]
     dat <- setNames(dat, c( "municipio", "age", "M", "F"))
     dat <- melt(dat,
                 id.vars = c( "municipio", "age"),
                 variable.name = "gender", 
                 value.name = "estimate")
  }
  
  return(dat)
}


get_wrangle_estimates <- function(year_input, product, municipio, census_key) {

  if (product == "acs") {
    state_code <- 72
    api <- paste0("https://api.census.gov/data/", year_input,
                  "/acs/acs5?get=group(B01001)&for=county:*&in=state:", 
                  state_code,"&key=")
    x <- GET(paste0(api, census_key))
    dat <- fromJSON(rawToChar(x$content), flatten=T) 
    vars <- fromJSON(paste0("https://api.census.gov/data/", year_input, "/acs/acs5/variables.json"), flatten = T)$variables
    var_names <- vars_names(vars)
    dat <- wrangle_dat(dat, product = "acs", variable_names = var_names)
  } else if (product == "decennial") {
      if (year_input %in%  c(2020)){ 
        api <- "https://api.census.gov/data/2020/dec/dhc?get=group(P12),NAME&for=county:*&in=state:72&key="
        x <- GET(paste0(api, census_key))
        dat <- fromJSON(rawToChar(x$content), flatten=T) 
        vars <- fromJSON(paste0("https://api.census.gov/data/", year_input, "/dec/dhc/variables.json"), flatten = T)$variables
        
      } else if (year_input %in% c(2010)) {
        api <- "https://api.census.gov/data/2010/dec/sf1?get=group(P12)&for=county:*&in=state:72&key="
        x <- GET(paste0(api, census_key))
        dat <- fromJSON(rawToChar(x$content), flatten=T) 
        vars <- fromJSON(paste0("https://api.census.gov/data/", year_input, "/dec/sf1/variables.json"), flatten = T)$variables
        
      } else if (year_input %in% c(2000)) {
        api <- "https://api.census.gov/data/2000/dec/sf1?get=group(P012),NAME&for=county:*&in=state:72&key="
        x <- GET(paste0(api, census_key))
        dat <- fromJSON(rawToChar(x$content), flatten=T) 
        vars <- fromJSON(paste0("https://api.census.gov/data/", year_input, "/dec/sf1/variables.json"), flatten = T)$variables
      }
    var_names <- vars_names(vars)
    dat <- wrangle_dat(dat, product = "decennial", variable_names = var_names, year.input = year_input)
  } else if (product == "pep") {
    if (year_input %in% 2000:2009) {
      api <- "https://api.census.gov/data/2000/pep/int_charage?get=GEONAME,SEX,AGE,AGE_DESC,POP,DATE_DESC,DATE_,LASTUPDATE&for=state:72&key="
      x <- GET(paste0(api, census_key))
      dat <- fromJSON(rawToChar(x$content), flatten=T)
    } else if (year_input %in% 2010:2019) {
      if (!municipio) {
        api <- "https://api.census.gov/data/2019/pep/charage?get=POP,AGE,SEX,DATE_CODE,DATE_DESC&for=state:72&key="
        x <- GET(paste0(api, census_key))
        dat <- fromJSON(rawToChar(x$content), flatten=T)
      } else {
        api <- "https://api.census.gov/data/2019/pep/charagegroups?get=NAME,POP,AGEGROUP,SEX,DATE_CODE,DATE_DESC&for=county:*&in=state:72&key="
        x <- GET(paste0(api, census_key))
        dat <- fromJSON(rawToChar(x$content), flatten=T)
      }
    } else if (year_input %in% 2020:2023) {
    url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/asrh/cc-est2023-syagesex.csv"
    dat <- read.csv(url)  
    }
    dat <- wrangle_pep(dat, municipio = municipio, year.input = year_input)
  }
  return(dat)
}

get_census_estimates <- function(years, product, municipio, census_key = census_key) {
    if (product == "decennial" & !all(years %in% c(2000, 2010, 2020))) {
     return(print("Decennial products are only available for 2000, 2010, 2020. Change years argument."))
    }
    if (product == "pep" & any(years <= 2019) & any(years >= 2020)) {
      return(print("Run two separate calls, one for years before 2020 and another one for 2020 onward."))
    }
   
  temp <- map_df(years, function(y) {
    tmp <- get_wrangle_estimates(
      product = product,
      year_input = y,
      municipio = municipio,
      census_key = census_key
     )
    tmp$year <- as.character(y)
    return(tmp)
  })
 return(temp)
}


pep <- get_census_estimates(2000:2019, product = "pep", municipio = FALSE, census_key = census_key)

pep2 <- get_census_estimates(2020:2023, product = "pep", municipio = FALSE, census_key = census_key)
pep2 <- pep2[, keyby = .(age, year, gender), .(estimate = sum(as.numeric(estimate)))]
pep <- data.table(rbind(pep, pep2))
save(pep, file = "../mortality/rdas/pep.rda")
## test 



