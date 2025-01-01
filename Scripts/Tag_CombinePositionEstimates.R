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


