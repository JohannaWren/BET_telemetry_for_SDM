# Add libraries
library(dplyr)
library(data.table)
library(here)
library(readxl)
library(here)
library(ggplot2)

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
  rename(tag.serial=tagID, lon=mptlon, lat=mptlat)
head(dataFinal)


# Replacing the id's to be consistent with Dan's tags
# Replace NA's with the other name so no NAs
idxVec2[which(idxVec2 == 'NA')] <- idxVec1[which(idxVec2 == 'NA')]
length(idxVec1)
length(idxVec2)

# # Replace all tag.serial numbers with the second TagID that's in the filename
# for (i in seq_along(myVec2)) {
#   idx <- which(myDF$tag.serial == myVec2[i])
#   myDF$tag.serial[idx] <- myVec1[i]
# }
# head(myDF)


#-------------------------------------------------------------------------------
#  IATTC FILES
#-------------------------------------------------------------------------------
# Read in files
danFiles <- list.files(path='Output', pattern = '.xlsx', full.names = T)
danData1 <- read_xlsx(danFiles[1], sheet = 2) %>% 
  select(dataname, year, month, day, mptlon, mptlat, mptsst, tagsst)
danData2 <- read_xlsx(danFiles[1], sheet = 4) %>% 
  select(dataname, year, month, day, mptlon, mptlat, mptsst, tagsst)
danData3 <- read_xlsx(danFiles[2]) %>% 
  select(dataname, year, month, day, mptlon, mptlat, mptsst) %>% 
  mutate(tagsst=NA)
danData4 <- read.csv('Output/tL98479a.csv') %>% 
  select(dataname, year, month, day, mptlon, mptlat, mptsst, tagsst)

# Combine all and rename columns
danData <- bind_rows(list(danData1, danData2, danData3, danData4), .id = 'id') %>% 
  rename(tag.serial=dataname, lon=mptlon, lat=mptlat)
head(danData)

#-------------------------------------------------------------------------------
# COMBINE ALL DATA
#-------------------------------------------------------------------------------
allData <- dataFinal %>% 
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

# Plot daily points
ggplot() +
  borders(database = 'world2', fill='black') +
  geom_point(data=allData, aes(lon, lat, group = year, color=factor(year)), alpha=0.5, size=1) +
  coord_quickmap(xlim=c(140,280), ylim=c(-30,40)) + 
  theme_bw()

# How many tags?
length(unique(allData$tag.serial))


