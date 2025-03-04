# Script to grab track data from the UKFSST output files and combine them into one file

# Add libraries
library(dplyr)
library(data.table)

# Set working directory
mainDir <- "~/Documents/TunaMovement/RAnalysis_Tags/"
setwd(mainDir)

# Get all filenames
folders <- c('Tags_KH_DI', 'SPC_Lotek', 'SPC_WC', 'New Archival Tag Data 2024')
allFolders <- paste0(folders, '/FinishedTags/')

# Read in all files into one dataframe
allFiles <- dir('Data/', recursive=T, full.names=T, pattern='_a.csv')
allFiles <- allFiles[-grep('OLD_', allFiles)]

dataList <- lapply(allFiles, read.csv)
dataDF <- rbindlist(dataList)

# Get the tag numbers from the file name
test <- transpose(strsplit(dataDF$dataname, split = '/'))[[2]]
tagID <- transpose(strsplit(test, '_'))[[3]]
dataDF$tagID <- tagID

# Get rid of all the unnecessary data and save just the tag number, date, location, and temperature.  
dataFinal <- dataDF %>% 
  select(tagID, year, month, day, mptlon, mptlat, mptsst, tagsst) %>% 
  rename(lon=mptlon, lat=mptlat, sst=mptsst)

  

