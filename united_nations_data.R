### United Nations Data ###

library(jsonlite)
library(httr)

# # Declares the base url for calling the API
# base_url <- "https://population.un.org/dataportalapi/api/v1"
# 
# # Creates the target URL, indicators, in this instance
# target <- paste0(base_url, "/indicators/")
# 
# # Get the response, which includes data as well as information on pagination and number of records
# response <- fromJSON(target)
# 
# # Get the first page of data
# df <- response$data
# 
# # Loop until there are new pages with data
# while (!is.null(response$nextPage)){
#   
#   #call the API for the next page
#   response <- fromJSON(response$nextPage)
#   
#   #add the data of the new page to the data.frame with the data of the precious pages
#   df <- rbind(df, response$data)
#   
# }

base_url_UNPD <- "https://population.un.org/dataportalapi/api/v1"

# 
# # Define the target url
# target <- paste0(base_url_UNPD, "/locations/")
# 
# # Call the API
# response <- fromJSON(target)
# 
# # Get the data
# df_geoarea_UNPD <- response$data
# 
# # Get the other pages with data
# while (!is.null(response$nextPage)){
#   
#   response <- fromJSON(response$nextPage)
#   df_geoarea_UNPD <- rbind(df_geoarea_UNPD, response$data)
#   
# }

puerto_rico_iso3 <- "PRI"
puerto_rico_iso2 <- "PR"
puerto_rico_id <- "630"
indicator_code <- "47"
start_year <- "2027"
end_year <- "2028"


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
