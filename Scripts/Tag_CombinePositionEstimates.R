# Add libraries
library(dplyr)
library(data.table)
library(here)
library(readxl)

# Set working directory
mainDir <- paste(here(), 'Output',sep='/')
setwd(mainDir)

#-------------------------------------------------------------------------------
# MY PROCESSED FILES
#-------------------------------------------------------------------------------
# Read in the files
myFiles <- list.files(path='FinalOutputTags/', full.names = T)
myVec <- lapply(strsplit(myFiles, '_'), "[", 3, drop=F) %>% 
  unlist()

# Remove duplicates 
# Some files have both fit1 and fit2 saved, and I wast to use only one model. Refer to my tag processing notes to see which is the best fit model. 
myFiles <- myFiles[-which(duplicated(myVec) | duplicated(myVec, fromLast = T))[2]]

# Read in files and make into one dataframe
myData <- lapply(myFiles, FUN=read.csv)
myDF <- rbindlist(myData)

# One file is in the wrong format so that needs to be fixed
names(myData[[1]])
names(myData[[63]])
myData[[63]] <- myData[[63]] %>% 
  mutate(tag.serial=strsplit(myFiles[63], '_')[[1]][2]) %>% 
  select(X, tag.serial, year, month, day, lon, lat, sst)

# Try again
myDF <- rbindlist(myData) %>% 
  mutate(X=NULL) %>% 
  data.frame()

#-------------------------------------------------------------------------------
#  IATTC FILES
#-------------------------------------------------------------------------------
# Read in files
dansFiles <- list.files(pattern = '.xlsx')
