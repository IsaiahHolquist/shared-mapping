############################################################################################
# Title: airtable_shared_summary.R
# Author: Isaiah
# Status: WIP
# Description: Script to get all shared pricing, circ, and holdout info from airtable for a
# specific client and update the map file accordingly
############################################################################################

# load in packages
library(tidyverse) # tidyverse for data manipulation
library(lubridate) # lubridate for date parsing
library(data.table) # data.table for writing results

# source airtable api script
source("matchbacks-processing/processing/airtable map summary/api_airtable_help.R")

# load in shared master table from airtable
# if it errors, likely caused by blank rows in airtable. Have to have Andrew to check for blank rows in airtable and delete them
shared_master <- airtable_get("appie1EhE3IYg2mMD", "Shared Master")

# get alphabetic list of all unique client names to make sure you filter on correct name
shared_master %>% select(partner1.clean,partner2.clean,partner3.clean,partner4.clean,partner5.clean,partner6.clean,
                         partner7.clean,partner8.clean,partner9.clean,partner10.clean,partner11.clean,partner12.clean) %>% 
  t %>% c %>% unique() %>% sort()

# set client name
client <- "Casper"

# filter shared master table to only include rows where the client name occurs
# and the mailing wasn't cancelled, select only cols of interest, and sort
client.cut <- shared_master %>% filter(Status != "CANCELLED") %>% 
  filter_at(vars(ends_with(".clean")), any_vars(grepl(client, .))) %>% 
  select(`Envelope Name`, `Data ID`, `Envelope Circ`, `In Home Date`, `P1 - Revenue`:`P12 - Revenue`,
         contains("Addons"), contains(".clean"), contains("PCPlus")) %>% arrange(campaignName.clean, `Envelope Name`, desc = T)

# get the corresponding partner number for client of interest (e.g., "partner 4)
# create a matrix of T/F values for whether or not a client name is the listed partner name for each row
partner_matrix <- as.data.frame(sapply(client.cut[,29:42], grepl, pattern = client))
# return the indices of all columns where the client is the listed partner
partner_true <- as.data.frame(which(partner_matrix == T, arr.ind = T)) %>% arrange(row, desc = T)
# get the actual airtable name (e.g., "Partner 6") where the client name appears
client.cut$partner_name <- names(partner_matrix)[partner_true$col]
# get cleaned partner names to pull other columns (e.g., "P6")
client.cut$short_name <- paste("P", gsub("[a-z.]", "", client.cut$partner_name), sep = "")

# check PCPlus rows for client
temp <- shared_master %>% filter(Status != "CANCELLED") %>% 
  filter_at(vars(ends_with(".clean")), any_vars(grepl(client, .))) %>% 
  arrange(campaignName.clean, `Envelope Name`, desc = T) # filter full shared master table to only rows including client

temp <- cbind(temp, client.cut[,c("partner_name", "short_name")])

temp <- temp %>% mutate(col_name = paste0("Partner ", gsub("P", "", short_name)))

temp$client_id <- NA
# loop through rows and get corresponding record ID for client
for(row_num in 1:nrow(temp)){
  # have to hardcode P1 otherwise P10-P12 will also be selected
  if(temp$short_name[row_num] == "P1"){
    id <- unlist(temp[row_num, "Partner 1 - Bedding"])
  }
  else{
    id <- unlist(temp[row_num,grepl(temp$col_name[row_num], names(temp))])[1]
  }
  temp$client_id[row_num] <- id
}

# filter shared master to only include PCPlus mailings that match client ID and are confirmed
# we will add these rows to the cleaned output from normal shared mailings later
# pcplus mailings are mapped twice so filter to only include pcplus mailkey rows
# not able to map pcplus holdouts yet
pcplus <- shared_master %>% filter(`PCPlus Client` %in% unique(temp$client_id)) %>%
  filter(grepl("PCPlus", `Envelope Name`))

# for each row, select all relevant columns and rename partner specific ones 
# then stack cleaned rows together to get final summary view

# initialize dataframe
cleaned.summary <- data.frame(`Data ID` = character(), `Envelope Name`=character(), campaignName.clean = character(), `Envelope Circ` = integer(),
                              Revenue = integer(), AB_splits = character(), `In Home Date` = as.Date(character()), `End Date` = as.Date(character()),
                              stringsAsFactors = F)
# loop through each row and pull only the relevant details from the correct partner
for(row_num in 1:nrow(client.cut)){
  row <- client.cut[row_num,]
  new_row <- row %>% select(`Data ID`, `Envelope Name`, campaignName.clean, `Envelope Circ`, names(client.cut)[grepl(paste("^",row$short_name, " ", sep = ""), names(client.cut))],
                            `In Home Date`)
  # clean new row names
  names(new_row)[1] <- "mailKey"
  names(new_row)[5] <- "Revenue"
  names(new_row)[6] <- "AB_splits"
  new_row$`End Date` <- ymd(new_row$`In Home Date`) + 89
  cleaned.summary <- rbind(cleaned.summary, new_row)
}

# convert NULL values to NA so results will write properly
cleaned.summary$AB_splits[which(cleaned.summary$AB_splits == "NULL")] <- NA

# filter out future mailings
cleaned.summary <- cleaned.summary %>% filter(date(unlist(`In Home Date`)) <= now())

# If no A/B split details in airtable, automatically combine splits (circ and cost)
# strip end of envelope name (" - 1A", "- 2" etc.) and group by envelope name
cleaned.summary <- cleaned.summary %>% mutate(`Envelope Name` = case_when(is.na(AB_splits)|!grepl("Unique|A/B", AB_splits) ~ gsub("- 1A|- 1B|- [0-9]", "", `Envelope Name`),
                                                                          T ~ `Envelope Name`))
cleaned.summary <- cleaned.summary %>% group_by(`Envelope Name`, campaignName.clean) %>% mutate(`Envelope Circ` = sum(`Envelope Circ`), Revenue = sum(Revenue))

# switch order of cost and circ columns to match mapper format
cleaned.summary <- cleaned.summary[,c(1,2,3,5,4,6,7,8)]

# create market ID from mailkey
cleaned.summary <- cleaned.summary %>% mutate(marketID = gsub(".*-(.+)-.*", "\\1", mailKey))

# add pcplus rows (if necessary) to existing df
# if pcplus rows exist, filter to only include cols of interest
if (nrow(pcplus) > 0){
  cols <- names(client.cut %>% select(-c("partner_name", "short_name", "Envelope Circ")))
  pcplus <- pcplus %>% select(all_of(cols), contains("PCPlus"))
  names(pcplus)[which(names(pcplus) == "Data ID")] <- "mailKey"
  names(pcplus)[which(names(pcplus) == "PCPlus Total Revenue")] <- "Revenue"
  names(pcplus)[which(names(pcplus) == "PCPlus Circ")] <- "Envelope Circ"
  
  # add end date and market date
  pcplus <- pcplus %>% mutate(`End Date` = ymd(`In Home Date`)+89, marketID = gsub(".*-(.+)-.*", "\\1", mailKey), AB_splits = NA)
  
  # select only relavent rows
  pcplus <- pcplus %>% select(all_of(names(cleaned.summary)))
  
  # update mailing name to indicate postcard plus mailing
  pcplus <- pcplus %>% mutate(campaignName.clean = paste0(gsub("[A-z]", "", campaignName.clean), "Postcard Plus"))
  
  # append pcplus data and reorder rows
  cleaned.summary <- rbind(cleaned.summary, pcplus)
  cleaned.summary <- cleaned.summary %>% arrange(campaignName.clean, `Envelope Name`, desc = T)
}

# read in holdouts mapper
marketIDs = fread("matchbacks-processing/processing/airtable map summary/citiesHoldout_marketID_2023-01-19.csv")
market_summ = fread("matchbacks-processing/processing/airtable map summary/citiesHoldout_summary_2023-01-19.csv")

# join market IDs to summary counts + filenames
markets.final22 <- marketIDs %>% left_join(market_summ, by = "Listcode")

# rename columns to match what is in cleaned.summary
names(markets.final22)[3] <- "mailKey"
names(markets.final22)[5] <- "Envelope Circ"

# read in final 2023 holdout counts
markets.final23 <- fread("matchbacks-processing/processing/airtable map summary/Holdout Counts 2023.csv")

# rename columns to keep consistent
markets.final23 <- markets.final23 %>% select(`Envelopes - Synced`, marketID, MISC, originalMailfile, n)
names(markets.final23)[1] <- "Name"
names(markets.final23)[3] <- "mailKey"
names(markets.final23)[4] <- "originalFileName"
names(markets.final23)[5] <- "Envelope Circ"

# change data types of columns
cleaned.summary$`In Home Date` <- ymd(cleaned.summary$`In Home Date`)
cleaned.summary$`End Date` <- ymd(cleaned.summary$`End Date`)

# split cleaned summary into separate dataframes for each mailing
cleaned.summary.split <- split(cleaned.summary, cleaned.summary$campaignName.clean)

# initialize dataframe that will hold appended mailed/holdout rows
final_df <- data.frame()
for(campaign in cleaned.summary.split){
  # check which version of holdouts to use
  if(unique(year(campaign$`In Home Date`)) == "2023"){markets.final <- markets.final23}
  else{markets.final <- markets.final22}
  
  # determine if mailing is cities or national mailing
  if (grepl("Cities|Postcard", unique(campaign$campaignName.clean))){
    holdout_markets <- markets.final %>% filter(marketID %in% unique(campaign$marketID)) %>% arrange(mailKey) # select only holdout markets in mailing
    
    # convert mailkey to all uppercase
    holdout_markets$mailKey <- toupper(holdout_markets$mailKey)
    
    # mutate holdout df to include additional columns (e.g., campaign name, IHD, etc.)
    holdout_markets <- holdout_markets %>% mutate(campaignName.clean = unique(campaign$campaignName.clean),
                                                  `Envelope Name` = mailKey,
                                                  `In Home Date` = date(unique(campaign$`In Home Date`)),
                                                  `End Date` = date(unique(campaign$`End Date`)))
    
    # assign status to rows
    campaign$Status <- "Mailed"
    holdout_markets$Status <- "Holdout"
    
    # assign priority to rows
    campaign$priority <- 1:nrow(campaign)
    holdout_markets$priority <- 0
    
    # append holdout markets to mailed markets
    campaign <- bind_rows(campaign, holdout_markets)
    # create mailing name
    diff <- ifelse(day(unique(campaign$`In Home Date`)) > 15, "Late", "Early") # differentiate between early and late cities mailings
    if(grepl("Cities",unique(campaign$campaignName.clean))){
      mailingName <- paste("Shared", diff, lubridate::month(date(unique(campaign$`In Home Date`)), label = T), lubridate::year(date(unique(campaign$`In Home Date`))),
                           "Cities", sep = " ")
    }
    else{
      mailingName <- paste("Shared", diff, lubridate::month(date(unique(campaign$`In Home Date`)), label = T), lubridate::year(date(unique(campaign$`In Home Date`))),
                           "Postcard Plus", sep = " ")
    }
    # add mailing name and targeting type as columns
    campaign <- campaign %>% mutate(Mailing = mailingName, Targeting = "Cities")
  }
  else{
    # create holdout rows for national segments
    # create one holdout row for each national segment
    holdout_natl <- data.frame()
    # loop through each mailed row and create corresponding holdout row
    for (index in 1:nrow(campaign)){
      row <- campaign[index,]
      # generate natl holdout row based on row (e.g., if "Moms" was mailed, create "H-Moms" testcell)
      holdout_row <- c(mailKey = sub("-", "-H-", row$mailKey),
                       campaignName.clean = unique(campaign$campaignName.clean),
                       `Envelope Name` = paste0("H-", gsub(" - [0-9][A-Z]?", "", row$`Envelope Name`)),
                       `Envelope Circ` = 10000,
                       marketID = row$marketID)
      
      # bind holdout rows together
      holdout_natl <- bind_rows(holdout_natl, holdout_row)
    }
    # assign status to rows
    campaign$Status <- "Mailed"
    holdout_natl$Status <- "Holdout"
    
    # assign priority to rows
    campaign$priority <- 1:nrow(campaign)
    holdout_natl$priority <- 0
    
    # sum up holdout circ by segment (Moms, Mens, etc.) since there will never be holdout A/B splits
    holdout_natl$`Envelope Circ` <- as.integer(holdout_natl$`Envelope Circ`)
    holdout_natl$`In Home Date` <- date(unique(campaign$`In Home Date`))
    holdout_natl$`End Date` <- date(unique(campaign$`End Date`))
    holdout_natl <- holdout_natl %>% group_by(marketID) %>% mutate(`Envelope Circ` = sum(`Envelope Circ`))
    
    # bind holdout rows to mailed rows
    campaign <- bind_rows(campaign, holdout_natl)
    # create mailing name
    mailingName <- paste("Shared", lubridate::month(date(unique(campaign$`In Home Date`)), label = T), lubridate::year(date(unique(campaign$`In Home Date`))),
                         "National", sep = " ")
    # add targeting type
    campaign <- campaign %>% mutate(Mailing = mailingName, Targeting = "National")
  }
  # bind mailing to final df
  final_df <- bind_rows(final_df, campaign)
}

# add original file name field if it doesn't exist (if a client has only run in national mailings)
if(!("originalFileName" %in% names(final_df))){
  final_df$originalFileName <- NA
}

# add additional columns and remove unnecessary ones to match format of existing mapper structure
final_df <- final_df %>% mutate(order = NA, testCell = toupper(`Envelope Name`), `Test Description`=toupper(`Envelope Name`), Format = NA, Paper = NA) %>% 
  select(order, priority, Status, originalFileName, mailKey, Mailing, testCell, Targeting, `Test Description`, Format, Paper, Revenue, `Envelope Circ`, `In Home Date`, `End Date`)

# read in existing map file (if it exists)
map_file <- list.files(pattern = "map_")

# check to see if the map file exists and update the airtable dataframe accordingly
if(length(map_file < 1)){
  # read in map file
  map <- fread(map_file)
  
  # convert IHD and end date to dates
  map$inHomeDate <- mdy(map$inHomeDate)
  map$endDate <- mdy(map$endDate)
  
  # calculate max IHD (have to exclude poplar IHDs)
  max_date <- map %>% filter(!grepl("triggered", originalFileName, ignore.case = T)) %>% select(inHomeDate)
  max_date <- max(max_date$inHomeDate)
  
  # filter airtable results to only include "new" mailings not already mapped
  final_df <- final_df %>% filter(`In Home Date` > max_date)
  
  # loop through rows and assign order to each row based on A/B splits
  order <- max(map$order) + 1
} else{order <- 1}

for (index in 1:nrow(final_df)){
  # use circ to determine A/B splits
  # take circ divided by 100k to determine number of times order should be repeated
  if (final_df$Status[index] == "Mailed" | final_df$Targeting[index] == "Cities"){len <- final_df[index,]$`Envelope Circ`/100000}
  else {len <- final_df[index,]$`Envelope Circ`/10000}
  
  # skip row if order already assigned
  if(!is.na(final_df$order[index])) next
  
  # assign order to rows
  if (len == 1){
    # check if 100k segment is actually part of 400k A/B split
    if (grepl("- [0-9]", final_df$testCell[index]) & final_df$Status[index] == "National") {
      # check to see if test cell at corresponding 400k spot is part of same segement
      if (gsub(" - [0-9]", "", final_df$testCell[index]) == gsub(" - [0-9]", "", final_df$testCell[index+3])){
        # assign A/B splits for 400k segment and update circ and revenue counts
        # assignments should always be 1+3 and 2+4, but technically could be different if special client request
        final_df$order[c(index, index+2)] <- order
        final_df$order[c(index+1, index+3)] <- order + 1
        order <- order + 2
      }
      else{
        # normal assignment
        final_df$order[index] <- order
        order <- order + 1
      }
    }
    # assign order for normal 100k splits
    else{
      final_df$order[index] <- order
      order <- order + 1
    }
  }
  else {
    # assign order
    index2 <- index+(len-1)
    final_df$order[index:index2] <- order
    order <- order + 1
  }
}

# read in all filenames from the mail files folder
project_folder <- gsub(".*\\/", "", getwd())
mailfiles <- list.files(path = "../../../PII/Mail Files", full.names = T, recursive = T)

for (row_num in 1:nrow(final_df)){
  if (final_df$Status[row_num] == "Mailed"){
    # check if mailing is postcard plus mailing
    if (grepl("Postcard Plus", final_df$Mailing[row_num])){
      # find mailfile that corresponds to mailing date and mailed file
      fileName <- sub(".*/", "", mailfiles[which(grepl(final_df$`In Home Date`[row_num], mailfiles) &
                                                   grepl("Postcard Plus|PCPlus|PC+|PCP",mailfiles,ignore.case = T) & 
                                                   grepl("campaign",mailfiles))])
      if(length(fileName) == 0){stop("No file name found. Check that mailing has been added to mail files folder!")}
      
      final_df$originalFileName[row_num] <- fileName
    }
    else{
      # find mailfile that corresponds to mailing date and mailed file
      fileName <- sub(".*/", "", mailfiles[which(grepl(final_df$`In Home Date`[row_num], mailfiles)&grepl("campaign",mailfiles))])
      if(length(fileName) == 0){stop("No file name found. Check that mailing has been added to mail files folder!")}
      final_df$originalFileName[row_num] <- fileName
    }
  }
  else{
    # find mailfile that corresponds to holdout date and holdout file
    fileName <- sub(".*/", "", mailfiles[which(grepl(final_df$mailKey[row_num], mailfiles,ignore.case = T)&grepl(final_df$`In Home Date`[row_num], mailfiles))])
    # have to check if holdout files are in combined aggregate file
    # this will be the case if the static holdout script is used to generate one large holdout file for a shared campaign
    if (length(fileName) == 0){
      fileName <- sub(".*/", "", mailfiles[which(grepl("StaticHoldouts", mailfiles,ignore.case = T)&grepl(final_df$`In Home Date`[row_num], mailfiles))])
    }
    final_df$originalFileName[row_num] <- fileName
  }
}

# convert dates to mdy
final_df$`In Home Date` <- format(final_df$`In Home Date`, "%m/%d/%y")
final_df$`End Date` <- format(final_df$`End Date`, "%m/%d/%y")

# rename columns to match map file
names(final_df)[5] <- "mailkey"
names(final_df)[12] <- "Cost"
names(final_df)[13] <- "Quantity"
names(final_df)[14] <- "inHomeDate"
names(final_df)[15] <- "endDate"

# append new rows to existing map file (if it exists)
if(length(map_file < 1)){
  # convert map dates back to mdy year format
  # the processing script expects these files in mdy format so must be hardcoded
  map$inHomeDate <- format(map$inHomeDate, "%m/%d/%y")
  map$endDate <- format(map$endDate, "%m/%d/%y")
  
  map <- rbind(map, final_df, fill = T)
} else{map <- final_df}

# write results
fwrite(map, paste0("map_", client, "_airtable.csv"))
