### Function to call PEP data ###
library(httr)
library(jsonlite)
library(tidyverse)
library(data.table)
library(stringr)

source("census_key.R", echo=TRUE)
source("helper_fncs.R")

## ACS
# 2009 - 2022 
# 3-25E Males
# 27-49E Females
year <- 2009
state_code <- 72
api <- paste0("https://api.census.gov/data/", year,
              "/acs/acs5?get=group(B01001)&for=county:*&in=state:", 
              state_code,"&key=")
x <- GET(paste0(api, census_key))
dat <- fromJSON(rawToChar(x$content), flatten=T) 
vars <- fromJSON(paste0("https://api.census.gov/data/", year, "/acs/acs5/variables.json"), flatten = T)$variables
var_names <- vars_names(vars)

dat <- wrangle_dat(dat, product = "acs", variable_names = var_names)

#### including moe 

wrangle_dat <- function(dat, product = NULL, variable_names = NULL, year.input = NULL) {
  product <- "acs"
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









## Decennial census 
# 2020 T02003 Due to adaptive design not every municipality is included 
# 3-25 males
# 27-49 female
year <- 2020
api <- "https://api.census.gov/data/2020/dec/ddhca?get=group(T02003),COUNTY&for=county:*&in=state:72&key="
x <- GET(paste0(api, census_key))
dat <- fromJSON(rawToChar(x$content), flatten=T) 
vars <- fromJSON(paste0("https://api.census.gov/data/", year, "/dec/ddhca/variables.json"), flatten = T)$variables
var_names <- vars_names(vars)

dat <- wrangle_dat(dat, product = "decennial", variable_names = var_names, year = 2020)

# 2010 P12
# 3-20 males
# 27-49 female
year <- 2010
api <- "https://api.census.gov/data/2010/dec/sf1?get=group(P12)&for=county:*&in=state:72&key="
x <- GET(paste0(api, census_key))
dat <- fromJSON(rawToChar(x$content), flatten=T) 
vars <- fromJSON(paste0("https://api.census.gov/data/", year, "/dec/sf1/variables.json"), flatten = T)$variables
var_names <- vars_names(vars)
dat <- wrangle_dat(dat, product = "decennial", variable_names = var_names, year = 2010)


# 2000 P012
# 3-25 males
# 27-49 female
year <- 2000
api <- "https://api.census.gov/data/2000/dec/sf1?get=group(P012),NAME&for=county:*&in=state:72&key="
x <- GET(paste0(api, census_key))
dat <- fromJSON(rawToChar(x$content), flatten=T) 
vars <- fromJSON(paste0("https://api.census.gov/data/", year, "/dec/sf1/variables.json"), flatten = T)$variables
var_names <- vars_names(vars)
dat <- wrangle_dat(dat, product = "decennial", variable_names = var_names, year = 2000)


## Population estimates and projections
# 2000-2010

# SYA: 
## DATE_1 =1 -> population base from 2000, 2 - 11 are pop estimates
api <- "https://api.census.gov/data/2000/pep/int_charage?get=GEONAME,SEX,AGE,AGE_DESC,POP,DATE_DESC,DATE_,LASTUPDATE&for=state:72&key="
x <- GET(paste0(api, census_key))
dat <- fromJSON(rawToChar(x$content), flatten=T)
# Agegroups: 2010/int_charagegroup [up to county (NOT AVAILABLE BY AGEGROUP)
api <- "https://api.census.gov/data/2000/pep/int_charagegroups?get=GEONAME,SEX,POP,DATE_DESC,DATE_&for=county:*&in=state:72&key="
x <- GET(paste0(api, census_key))
dat <- fromJSON(rawToChar(x$content), flatten=T)

# 2010 - 2014
## can only input years 2013-2014
# #SYA
# api <- "https://api.census.gov/data/2014/pep/prcagesex?get=POP,SEX,AGE,DATE_&for=state:72&&key="
# x <- GET(paste0(api, census_key))
# data_census_x <- fromJSON(rawToChar(x$content), flatten=T)
# 
# # municipio  https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars.Vintage_2013.html
# api <- "https://api.census.gov/data/2014/pep/prmagesex?get=CTYNAME,AGEGRP,POP,SEX,STNAME,DATE_&for=county:*&in=state:72&key="
# x <- GET(paste0(api, census_key))
# data_census_x <- fromJSON(rawToChar(x$content), flatten=T)
# 
# # 2015-2018
# #SYA
# api <- "https://api.census.gov/data/2015/pep/charage?get=POP,AGE,SEX&for=state:72&key="
# x <- GET(paste0(api, census_key))
# data_census_x <- fromJSON(rawToChar(x$content), flatten=T)
# #Muni
# 
# api <- "https://api.census.gov/data/2015/pep/charagegroups?get=GEONAME,POP,AGEGROUP,SEX&for=county:*&in=state:72&key="
# x <- GET(paste0(api, census_key))
# data_census_x <- fromJSON(rawToChar(x$content), flatten=T)

#2010-2019
# SYA
api <- "https://api.census.gov/data/2019/pep/charage?get=POP,AGE,SEX,DATE_CODE,DATE_DESC&for=state:72&key="
x <- GET(paste0(api, census_key))
dat <- fromJSON(rawToChar(x$content), flatten=T)
# muni 
api <- "https://api.census.gov/data/2019/pep/charagegroups?get=NAME,POP,AGEGROUP,SEX,DATE_CODE,DATE_DESC&for=county:*&in=state:72&key="
x <- GET(paste0(api, census_key))
data_census_x <- fromJSON(rawToChar(x$content), flatten=T)

# 2020 - onward (not available through apis)

# 2020-2023
url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/asrh/cc-est2023-syagesex.csv"
dat <- read.csv(url)  
dat_pep <- wrangle_pep(dat,  year = 2021)
# muni 

url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/asrh/cc-est2023-agesex-72.csv"
