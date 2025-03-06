# Add libraries
library(dplyr)
library(data.table)
library(here)
library(readxl)
library(here)

# Set working directory
mainDir <- here()
setwd(mainDir)

#-------------------------------------------------------------------------------
# MY PROCESSED FILES
#-------------------------------------------------------------------------------
# Read in all files into one dataframe
allFiles <- dir('Data/AllFinishedTags', recursive=T, full.names=T, pattern='_a.csv')
# Check for duplicates
idx <- transpose(strsplit(allFiles, '/'))[[3]]
idxVec1 <- transpose(strsplit(idx, '_'))[[3]]
idxVec2 <- transpose(strsplit(idx, '_'))[[2]]
duplicated(idxVec1) | duplicated(idxVec1, fromLast = T)
# Remove duplicates if any
which(duplicated(idxVec1) | duplicated(idxVec1, fromLast = T))
allFiles <- allFiles[-c(48,49)]
idxVec1 <- idxVec1[-c(48,49)]
idxVec2 <- idxVec2[-c(48,49)]

# Read in all files into a list
dataList <- lapply(allFiles, read.csv)
# Add tagID to the files
dataList2 <- mapply(cbind, dataList, tagID=idxVec1, SIMPLIFY=F)
# Put into one dataframe
dataDF <- rbindlist(dataList2)
head(dataDF)

# Get rid of all the unnecessary data and save just the tag number, date, location, and temperature.  
dataFinal <- dataDF %>% 
  select(tagID, year, month, day, mptlon, mptlat, mptsst, tagsst) %>% 
  rename(lon=mptlon, lat=mptlat, sst=mptsst)


# Replacing the id's to be consistent with Dan's tags
# Get all tag names and ids
myVec1 <- lapply(strsplit(myFiles, '_'), "[", 3, drop=F) %>% 
  unlist()
myVec2 <- lapply(strsplit(myFiles, '_'), "[", 2, drop=F) %>% 
  unlist()
# Replace NA's with the other name so no NAs
myVec2[which(myVec2 == 'NA')] <- myVec1[which(myVec2 == 'NA')]
length(myVec1)
length(myVec2)

# Replace all tag.serial numbers with the second TagID that's in the filename
for (i in seq_along(myVec2)) {
  idx <- which(myDF$tag.serial == myVec2[i])
  myDF$tag.serial[idx] <- myVec1[i]
}
head(myDF)


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
dupes <- allData %>% 
  group_by(tag.serial, id) %>% 
  distinct(tag.serial) %>% 
  data.frame()
dupesIdx <- which(duplicated(dupes$tag.serial) | duplicated(dupes$tag.serial, fromLast=T))
dupes[dupesIdx,]

# Remove duplicates
# Keep Dan's and remove mine. 
allData <- allData[-which(allData$tag.serial %in% dupes$tag.serial[dupesIdx] & allData$id == 5),] %>% 
  mutate(id=NULL)

write.csv(file = 'Tag_BET_All_MPTs.csv', x = allData, quote = F, row.names = F)

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


