# Functions for working with the Airtable API

# to load function(s) into your environment, add a line to
# source() this script in whatever script you're using

# store your APIkey as an object in the environment file
# in your system environment you have your API keys defined as thus:
## airtable_api_key

# file.edit("~/.Renviron")

# test api key is in your environment:
if(Sys.getenv("airtable_api_key") == ""){
  stop('You need to define airtable_api_key in the system environment.
       Run file.edit("~/.Renviron") and then add airtable_api_key = {YOUR API KEY}')
}

# Function: airtable_get --------------------------------------------------

# GET a table from airtable, in dataframe format.
# offset pagination, need to loop to get all data

airtable_get <- function(baseID, tableName, queryParameter = NULL) {
  library(tidyverse)
  library(httr)
  library(jsonlite)
  
  # first page:
  r <- GET(URLencode(paste0("https://api.airtable.com/v0/", baseID, "/", tableName)),
           query = c(queryParameter,
                     list(api_key = Sys.getenv("airtable_api_key"))))
  
  payload <- fromJSON(content(r, "text"))
  
  # initialize dataframe
  recordsDF <- payload$records$fields
  
  # While the current page has results, store its contents 
  # then advance to the next page.
  while(length(payload$offset) > 0) {
    # API has rate limit of five requests per second
    Sys.sleep(.20)
    
    # get the next page
    r <- GET(URLencode(paste0("https://api.airtable.com/v0/", baseID, "/", tableName)),
             query = c(queryParameter,
                       list(api_key = Sys.getenv("airtable_api_key"),
                          offset = payload$offset)))
    
    payload <- fromJSON(content(r, "text"))
    
    # stack into one dataframe
    recordsDF <- bind_rows(recordsDF, payload$records$fields)
  }
  
  # exeunt
  return(recordsDF)
}
