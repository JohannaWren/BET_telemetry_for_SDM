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
danFiles <- list.files(pattern = '.xlsx')
danData1 <- read_xlsx(danFiles[1], sheet = 2) %>% 
  select(dataname, year, month, day, mptlon, mptlat, tagsst)
danData2 <- read_xlsx(danFiles[1], sheet = 4) %>% 
  select(dataname, year, month, day, mptlon, mptlat, tagsst)
danData3 <- read_xlsx(danFiles[2]) %>% 
  select(dataname, year, month, day, mptlon, mptlat, mptsst) %>% 
  rename(tagsst=mptsst)
danData4 <- read.csv('tL98479a.csv') %>% 
  select(dataname, year, month, day, mptlon, mptlat, tagsst)

# Combine all and rename columns
danData <- bind_rows(list(danData1, danData2, danData3, danData4), .id = 'id') %>% 
  rename(tag.serial=dataname, lon=mptlon, lat=mptlat, sst=tagsst)


#-------------------------------------------------------------------------------
# COMBINE ALL DATA
#-------------------------------------------------------------------------------
allData <- myDF %>% 
  bind_cols(id='5') %>% 
  bind_rows(danData)


# duplicates?
dupes <- danData %>% 
  group_by(dataname, id) %>% 
  distinct(dataname) %>% 
  data.frame() %>% 
  bind_rows(data.frame(id='5', dataname=unique(myVec)))
dupesIdx <- which(duplicated(dupes$dataname) | duplicated(dupes$dataname, fromLast=T))

#-------------------------------------------------------------------------------
# CHECK DATA AND GET METRICS
#-------------------------------------------------------------------------------
# Plot tracks
ggplot() + 
  borders(database = 'world2', fill='black') + 
  geom_path(data=allData, aes(lon, lat, group=tag.serial, color=tag.serial)) + 
  coord_quickmap(xlim=c(140,280),ylim=c(-30,40)) + 
  theme_bw()

# How many tags?
length(unique(allData$tag.serial))


