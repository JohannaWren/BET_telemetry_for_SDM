title: "UKFSST Processing Script"
author: "Molly Scott"
date: "31/05/2023"
notes: "Johanna made changes to streamline reading in tag data and read some metadata informatin so we can 
fill the release and pop-off sites, and added Mitch's version of ukfsst script below"

#-------------------------------------------------------------------------------
# Read libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(ukfsst)
library(date)
library(kftrack)


rm(list=ls())
# Set working directory
#mainDir <- "C:\\Users\\Johanna.Wren/Desktop/SPC_WC/"
mainDir <- "C:\\Users\\Johanna.Wren/Desktop/New Archival Tag Data 2024/"
setwd(mainDir)

#-------------------------------------------------------------------------------
################## Format and clean DAP data for UKFSST ########################
#-------------------------------------------------------------------------------
# Set tag PTT number
tag <- '1890056'

#-------------------------------------------------------------------------------
meta <- read.csv('../Updated Archival Tag Data 2024.csv', na.strings=c(""," ","NA"))
#meta <- read.csv('../WLC_Tags_fromJoeJan2023.csv', na.strings=c(""," ","NA"))
#meta <- read.csv('../ArchivalTag_1MonthPlus_Summary.csv', na.strings=c(""," ","NA"))
#meta <- read.csv('../Tuna_metadata - ALL BET and YFT metadata.csv', na.strings=c(""," ","NA"))
#  Pull out the metadata record for your tag
tagMeta <- meta %>%
  filter(Arc_tag_no == tag)
# Set release and pop-off locations
tagMeta$DeployDate <- as.Date(tagMeta$release_date, format='%d/%m/%Y') #event_date, format='%Y-%m-%d')
tagMeta$RecoverDate <- as.Date(tagMeta$best_catchdate, format='%d/%m/%Y') #%Y-%m-%d
# tagMeta$DeployDate <- as.Date(ifelse(is.na(tagMeta$Tagging.DeployDateUTC),
#                                      tagMeta$Tagging.DeployDateLocal,
#                                      tagMeta$Tagging.DeployDateUTC),
#                               format='%d/%m/%Y')
#format='%d-%b-%Y')
#tagMeta$RecoverDate <- as.Date(tagMeta$PopoffDate, format='%d-%b-%Y')
# tagMeta$RecoverDate <- as.Date(tagMeta$Capture.FateDate, format='%d/%m/%Y')
tagMeta
# Print out tagging and recapture data for easy DAP access
#matrix(tagMeta[,c('DeployDate','Rel_Lon', 'Rel_Lat', 'RecoverDate', 'Rec_Lon', 'Rec_Lat')],nrow = 2, byrow = T)
matrix(tagMeta[,c('DeployDate','release_lon', 'release_lat', 'RecoverDate', 'recov_lon', 'recov_lat')],nrow = 2, byrow = T)

# Read in tag and metadata
#tagFiles <- list.files(path=paste0('ExtractedTags_SPC_WC/',tag), full.names = T)
tagFiles <- list.files(path=paste0('WildLife/Extracted/'), full.names = T)
tagFiles
tagLoc1 <- read.csv(tagFiles[grep('Locations', tagFiles)], na.strings=c(""," ","NA"))
head(tagLoc1)
tagSST1 <- read.csv(tagFiles[grep('LightLoc', tagFiles)], na.strings=c(""," ","NA"))
head(tagSST1)

# convert dates to appropriate formats
tagLoc1$YMD <- as.Date(tagLoc1$Date, format='%H:%M:%S %d-%b-%Y')
tagLoc1$YMDHMS <- as.POSIXct(tagLoc1$Date, format='%H:%M:%S %d-%b-%Y')
tagSST1$YMD <- as.Date(tagSST1$Day, format='%d-%b-%Y')

# Make Tag type a factor (not really sure why we are doing this)
tagSST1$Type = as.factor(tagSST1$Type)

#-------------------------------------------------------------------------------
# Reduce the SST record to dawn locations only during the time the tag was deployed (removes excessive start times)
tagSST = tagSST1 %>% 
  filter(Source == 'Transmission') %>% 
  dplyr::select(DeployID, Ptt, Type, YMD, SSTTemp) %>%
  filter(Type == "Dawn", YMD <= tagMeta$RecoverDate) 

# Remove any locations BEFORE the deploy date. 
# Sometimes there can be residual locations from when the tag was set up etc. and we want to remove those
if (min(tagLoc1$YMD) < tagMeta$DeployDate) { 
    tagLoc1 <- tagLoc1[-which(tagLoc1$YMD == min(tagLoc1$YMD)),]
}

# Reduce locations to once per day (morning only) and add deploy and pop-off locations
tagLoc <- tagLoc1 %>% # tagLoc1[!is.na(tagLoc1$Error.Semi.major.axis),] %>%   #for tag 1690142
  group_by(YMD) %>% 
  filter(YMDHMS == min(YMDHMS)) %>% 
  distinct() %>% 
  ungroup() %>%
  dplyr::select(YMD, Longitude, Latitude) %>% 
  add_row(YMD=tagMeta$DeployDate, Longitude=tagMeta$Rel_Lon, Latitude=tagMeta$Rel_Lat, .before = T) %>%
  #add_row(YMD=tagMeta$DeployDate, Longitude=tagMeta$Longitude...180., Latitude=tagMeta$Latitude, .before = T) %>%
  #add_row(YMD=tagMeta$DeployDate, Longitude=tagMeta$Longitude...180., Latitude=tagMeta$Tagging.DeployLatitude, .before = T) %>%
  filter(YMD <= tagMeta$RecoverDate) %>% 
  arrange(YMD) %>% 
  data.frame()

# Check to see if you need to add the last day or just change location
tagMeta$DeployDate
head(tagLoc)
tail(tagLoc)
data.frame(Date=tagMeta$RecoverDate, Lon=tagMeta$recov_lon, Lat=tagMeta$recov_lat)
#data.frame(Date=tagMeta$RecoverDate, Lon=tagMeta$Rec_Lon, Lat=tagMeta$Rec_Lat)
#data.frame(Date=tagMeta$RecoverDate, Lon=tagMeta$RecoveryBestLon.180, Lat=tagMeta$RecoveryBestLat)
#data.frame(Date=tagMeta$RecoverDate, Lon=tagMeta$Recovery.PopoffLong, Lat=tagMeta$Recovery.PopoffLat)
##
##
# DO ONE OR THE OTHER HERE, OR NOTHING
# If tag released instantly add pop-off location
#tagLoc[nrow(tagLoc),c('Longitude', 'Latitude')] <- as.numeric(tagMeta[,c('RecoveryBestLon.180', 'RecoveryBestLat')])
tagLoc[nrow(tagLoc),c('Longitude', 'Latitude')] <- as.numeric(tagMeta[,c('Rec_Lon', 'Rec_Lat')])
tail(tagLoc)
# If tag bobbed for a while, remove everything after the fate date and set fix.last=F in UKFSST
#tagLoc <- filter(tagLoc, YMD <= as.Date(tagMeta$Capture.FateDate, '%d-%b-%Y'))
tagLoc <- filter(tagLoc, YMD <= as.Date(tagMeta$RecoverDate, '%d/%m/%Y'))
tail(tagLoc)
##
tagLoc <- tagLoc[-2,]
#tagLoc[1,] <-  tagMeta[, c("DeployDate", "Longitude...180.", "Latitude")]
tagLoc[1,2:3] <-  tagMeta[, c("release_lon", "release_lat")]
##
## Add recover date and location to the end
#tagLoc <- tagLoc %>% add_row(YMD=tagMeta$RecoverDate, Longitude=tagMeta$Rec_Lon, Latitude=tagMeta$Rec_Lat)
tagLoc <- tagLoc %>% add_row(YMD=tagMeta$RecoverDate, Longitude=tagMeta$recov_lon, Latitude=tagMeta$recov_lat)
tagLoc
# or if you just need to put the recovery locations on the last date/row
tagLoc[nrow(tagLoc),'Longitude'] <- tagMeta$Rec_Lon
tagLoc[nrow(tagLoc),'Latitude'] <- tagMeta$Rec_Lat
tagLoc

# Merge locations and SST into one file with daily records
tagData <- tagLoc %>% 
  full_join(tagSST[,c('SSTTemp', 'YMD')]) %>% 
  mutate(day=day(YMD), month=month(YMD), year=year(YMD), Long=Longitude, Lat=Latitude, sst=SSTTemp) %>% 
  dplyr::select(day, month, year, Long, Lat, sst) 
# Remove SST from recover location
tagData$sst[nrow(tagData)] <- NA
# See what it looks like
tagData

# Clean up dataframe
# Change longitudes to 360 for UKFSST
tagData$Long <- ifelse(tagData$Long < 0, tagData$Long+360, tagData$Long)
# Remove records with missing values
which(tagData$Long == 0 & tagData$Lat == -80)
tagData <- tagData[-which(tagData$Long == 0 & tagData$Lat == -80),]
# Remove all records with more than one NA
which(rowSums(is.na(tagData))>1)
tagData <- tagData[-which(rowSums(is.na(tagData))>1),] 

tagData

#-------------------------------------------------------------------------------
# Plot the track so you can see wrong points
library(ggplot2)
ggplot(tagData, aes(Long, Lat, color=sst)) + 
  borders('world2', fill='gray') +
  geom_path() + 
  geom_point() +
  geom_point(data=tagData[c(1,nrow(tagData)),], aes(Long, Lat), size=3) +
  coord_quickmap(xlim = c(min(tagData$Long,na.rm=T)-5, max(tagData$Long,na.rm=T)+5), ylim = c(min(tagData$Lat,na.rm=T)-5, max(tagData$Lat,na.rm=T)+5)) + 
  scale_color_viridis_c(option='turbo')

# Remove any points that are way off. This will usually be the latitudes, the longitudes tend to be much better.
# If you have a 'bad' point, just remove the lat but leave the longitude in there. 
# Trying to come up with a less subjective way to clean the data so I wrote some code that removes points that are farther away
# than a bigeye can reasonable swim in the time between points. Doing this for both long and lats
# Distance calculations
library(geosphere)
library(scales) 
BETswimSpeed <- 3  # in m/s
times <- as.Date(do.call(sprintf, c(tagData[,3:1], '%s-%s-%s')))
timeScale <-  rescale(times, to=c(0,(times[length(times)]-times[1])))

# Loop through the lats and calculate distance between consecutive lats, assuming a constand longitude for ease
for (i in seq_along(timeScale[-1])) {
  j=i+1
  # If there are NAs that we introduce, we make sure to always use the point before the NAs start
  if (is.na(tagData$Lat[i])) {
    i=tail(which(!is.na(tagData$Lat[1:i])),n=1)
  }
  # Calculate distance between today and the next time step
  dist <- distMeeus(data.frame(Long=205,Lat=tagData[c(i,j),5]))
  totdist <- (dist > (BETswimSpeed*86400*diff.Date(times[c(i,j)])))
  # If the distance is too long for what a BET can realistically swim in that amount of time, set the Lat value to NA
  if (totdist == T) { tagData$Lat[j] <- NA}
}

# And doing the same for the longs, assuming a constant latitude for ease
for (i in seq_along(timeScale[-1])) {
  j=i+1
  # If there are NAs that we introduce, we make sure to always use the point before the NAs start
  if (is.na(tagData$Long[i])) {
    i=tail(which(!is.na(tagData$Long[1:i])),n=1)
  }
  # Calculate distance between today and the next time step
  dist <- distMeeus(data.frame(Long=tagData[c(i,j),4], Lat=0))
  totdist <- (dist > (BETswimSpeed*86400*diff.Date(times[c(i,j)])))
  # If the distance is too long for what a BET can realistically swim in that amount of time, set the Lon value to NA
  if (totdist == T) { tagData$Long[j] <- NA}
}

# See how it looks
ggplot(tagData, aes(timeScale, Lat)) +
  geom_path() +
  geom_point(size=2) 

ggplot(tagData, aes(timeScale, Long)) +
  geom_path() +
  geom_point(size=2) 

ggplot(tagData, aes(timeScale, sst)) +
  geom_path() +
  geom_point(size=2) 

#-------------------------------------------------------------------------------
# Clean up records for each tag



# # Tag 1890056
# tagData$Lat[which(tagData$Lat > 15 | tagData$Lat < -15)] <- NA
# tagData$Long[c(14:15,19,28,29,84)] <- NA
#tagData$Lat[which(tagData$Lat < 10)] <- NA
#tagData$Lat[which(tagData$Lat > 35)] <- NA
#Tag 168581
#tagData$Lat[which(tagData$Lat > 30)] <- NA
# Tag 168585
#tagData <- tagData[which(tagData$Long >= 220),]
#tagData$Lat[which(tagData$Lat <= 20)] <- NA
# Tag 168587
# tagData$Lat[which(tagData$Lat < 15)] <- NA
# tagData$Lat[which(tagData$Lat > 50)] <- NA
# tagData$Long[which(tagData$Long > 236)] <- NA
# tagData$Long[50] <- NA
# tagData$Long[44] <- NA
# Tag 0990298
# tagData$Lat[which(tagData$Lat <= -25 | tagData$Lat > 30)] <- NA
# tagData$Lat[166] <- NA
# tagData$Long[which(tagData$Long < 210)] <- NA
# tagData$sst[1] <- NA
# Tag 0890209
# tagData$Lat[which(tagData$Lat > 13 | tagData$Lat < -10)] <- NA
# tagData$Long[which(tagData$Long > 147)] <- NA
# tagData$sst[which(tagData$sst < 28.6)] <- NA
# tagData$sst[c(14,16,23)] <- NA
# Tag 0890010
# tagData$Lat[which(tagData$Lat > 10 | tagData$Lat < -10)] <- NA
# tagData$sst[nrow(tagData)] <- NA
# Tag 1290657
# tagData$Lat[which(tagData$Lat < -25 | tagData$Lat > 25)] <- NA
# tagData$Long[which(tagData$Long > 210)] <- NA
# tagData$Long[c(71,85,87,92,106,111)] <- NA
# tagData$sst[which(tagData$sst < 26.5)] <- NA
# Tag 14290659
# tagData$Lat[which(tagData$Lat > 15)] <- NA
# tagData$sst[nrow(tagData)] <- NA
# Tag 189081
# tagData$Lat[which(tagData$Lat > 12)] <- NA
# tagData$Lat[which(tagData$Lat < -10)] <- NA
# tagData$Long[which(tagData$Long < 188)] <- NA
# tagData[115,'Long'] <- NA
# tagData[5:6,'Long'] <- NA
# Tag 1090387
# tagData$Lat[which(tagData$Lat > 20)] <- NA
# tagData$Lat[which(tagData$Lat < -25)] <- NA
# tagData[108,'Lat'] <- NA
# tagData[159,'Long'] <- NA
# tagData[c(209:216, 247:249),'sst'] <- NA
# Tag 1190070
# tagData$sst[nrow(tagData)] <- NA
# tagData$Long[which(tagData$Long > 191)] <- NA
# tagData$Lat[which(tagData$Lat > 1 | tagData$Lat < -6)] <- NA
# Tag 1490057
# tagData <- tagData[1:117,]
# tagData$Long[which(tagData$Long < 195)] <- NA
# tagData$Lat[which(tagData$Lat > 25 | tagData$Lat < -25)] <- NA
# tagData$sst[which(tagData$sst < 28.8)] <- NA
# Tag 1490061
# tagData$Lat[which(tagData$Lat > 25 | tagData$Lat < - 13)] <- NA
# tagData$Long[c(9,10,19)] <- NA
# Tag 1490065
# tagData$Lat[which(tagData$Lat > 5 | tagData$Lat < -13)] <- NA
# tagData$Lat[c(17,35,180:184,186,187,188,170:173,177)] <- NA
# Tag 1590052
# tagData$Lat[which(tagData$Lat > 10 | tagData$Lat < -11.5)] <- NA
# tagData$Lat[c(6:8,11,14,106)] <- NA
# tagData$sst[which(tagData$sst <= 29)] <- NA
# tagData$sst[c(32:33,57)] <- NA
# tagData$Long[c(11:14,27)] <- NA
# tag 1690100
# tagData$Lat[which(tagData$Lat > 9)] <- NA
# Tag 1690142
# tagData$Lat[which(tagData$Lat > 0)] <- NA
# tagData$Lat[which(tagData$Lat < -15)] <- NA
# Tag 1690171
# tagData$Lat[which(tagData$Lat >= 0 | tagData$Lat <= -20)] <- NA
# tagData$Lat[c(5,6,47,81,97,113,111,118,121,125:128,132,133,135,137:140,181,250,268,283,288,298,304,343,347,341,353,374:377,388,389,397,398,401,412,425,427,431,434,436,438,443:445,447,448,456,458:460,463:469,489,490,492,499,503:505,508,514,518,523,529,530,533,534,536,540,544,545)] <- NA
# tagData$Long[which(tagData$Long < 155)] <- NA
# tagData$Long[c(47,74,78,81,97,89,118,120,121,428,353,386,388,483,517,529,530,533,539,541,547)] <- NA
# tagData$sst[202] <- NA
# Tag 1690174
# tagData$sst[which(tagData$sst < 29.5)] <- NA
# tagData$Long[which(tagData$Long < 166)] <- NA
# tagData$Lat[which(tagData$Lat > 0 | tagData$Lat < -25)] <- NA
# Tag 179005
# tagData <- tagData[-105,]
# tagData$sst[104] <- NA
# tagData$Lat[which(tagData$Lat > 0 | tagData$Lat <= -15)] <- NA
# tagData$Long[which(tagData$Long < 160)] <- NA
# tagData$Long[c(6,33,67,68)] <- NA
# Tag 1790072
# tagData$Lat[which(tagData$Lat < -25 | tagData$Lat > 10)] <- NA
# tagData$Long[which(tagData$Long < 172.5)] <- NA
# Tag 1790165
# tagData$Lat[which(tagData$Lat < -10 | tagData$Lat > 30)] <- NA
# tagData$Long[which(tagData$Long > 181 | tagData$Long < 173)] <- NA
# Tag 1790319
# tagData$Lat[which(tagData$Lat > 6 | tagData$Lat < -13)] <- NA
# tagData$Lat[c(64,70,72,73,116,120,130,149,151)] <- NA
# Tag 1990092
#tagData$Lat[which(tagData$Lat > 10 | tagData$Lat < -5)] <- NA



# Remove all records with more than one NA
which(rowSums(is.na(tagData))>1)
tagData <- tagData[-which(rowSums(is.na(tagData))>1),] 
# Save the final tag file for use in UKFSST
# The file need: day month, year, lon, lat, sst
write.csv(tagData, paste('WildLife/Finished/BET', tagMeta$tag_no, tag, 'LocSST_forUKFSST_cleaned.csv', sep='_'), row.names = F, quote = F)
#write.csv(tagData, paste('FinishedTags_SPC_WC/BET', tagMeta$tag_no, tag, 'LocSST_forUKFSST_cleaned.csv', sep='_'), row.names = F, quote = F)



#-------------------------------------------------------------------------------
############################## RUN UKFSST ######################################
#-------------------------------------------------------------------------------
# The below code is from Mitch Lowell when he was at IATTC. I have copied with no changes on my end. 
# I did rewrite the get.sst.from.server function (now called get.sst.from.server.jlkw) so that it works without an internet
# Connection. I have saved an SST file that is global and covers the date ranges of the tag. Instead of the function 
# querying the ERDAP server, it just queries the downloaded file. All else is the same, and more detailed notes on what 
# I changed can be found in the get.sst.from.server.jlkw script. 
#-------------------------------------------------------------------------------
# Load library
library(ukfsst)

# load supporting code
source("../fit2csv.R")
source("../get.sst.from.server_downloaded.R") # HIGH Resolution (can be adjusted in the get.sst.from.server source file)

# Get the SST values
# Start with 'high' resolution and if it doesn't converge, move to 'low'
sst.path <- suppressWarnings(get.sst.from.server.jlkw(na.omit(tagData), res='high'))

#-------------------------------------------------------------------------------
# Run the model
#-------------------------------------------------------------------------------
### Initial model - where bx.a and bsst.a = F
### We start here because the archival tags are really good at predicting
### longitude and sst; therefore, we don't want to bias these parameters
fit.1 <- kfsst(tagData,
               fix.last = F,
               u.a = T, 
               v.a = T, 
               D.a = T, 
               bx.a = F, 
               by.a = T, 
               bsst.a = F,
               sx.a = T,
               sy.a = T,
               ssst.a = T)




### See if model converged
print.kfsst(fit.1)
### Is the fit good?
plot.kfsst(fit.1, ci=T)


### Did the model converge? Is the fit good?
### Are there any spurious data points that need to be removed?


### Here, I turn off by.a to see if model performs better
fit.2 <- kfsst(tagData,
               fix.last = F,
               u.a = T, 
               v.a = T, 
               D.a = T, 
               bx.a = T, 
               by.a = T, 
               bsst.a = F,
               sx.a = T,
               sy.a = T,
               ssst.a = T)

### See if model converged
print.kfsst(fit.2)
### Is the f### Is the f### Is the fit good?
plot.kfsst(fit.2, ci=T)


# which model has best negative log likelihood?
fit.1$nlogL
fit.2$nlogL

### Did the model converge? Is the fit good?
### Are there any spurious data points that need to be removed?


### Here, I turn by.a back ON and assign starting parameters to see if model 
### performs better
fit.3 <- kfsst(tagData,
               fix.last = T,
               u.a = T, 
               v.a = T, 
               D.a = T, 
               bx.a = F, 
               by.a = T, 
               bsst.a = F,
               sx.a = T,
               sy.a = T,
               ssst.a = T,
               u.i = -3.240019  , 
               v.i = 2.545434  , 
               D.i = 1523.694  ,
               bx.i = -3.280755  , 
               by.i = -0.6426635        ,
               bsst.i = 0,
               sx.i = 2.057993  , 
               sy.i = 3.398656  , 
               ssst.i = 0.3656373        , 
               a0.i = 1.907377e-09 , 
               b0.i = 27.6435)

### See if model converged
print.kfsst(fit.3)
### Is the fit good?
plot.kfsst(fit.3, ci=T)



#-------------------------------------------------------------------------------
############################ Export csv file ###################################
#-------------------------------------------------------------------------------
### Now, let's format our model into the csv source files
setwd(mainDir)
SSTres <- 'high'
finMod <- fit.2
finVar <- 'fit2'
fit2csv(fit = finMod, name = paste('WildLife/Finished/BET', tagMeta$tag_no, tag, 'UKFSST_BestFitModel', SSTres, finVar, '', sep='_'))
saveRDS(finMod, paste('WildLife/Finished/BET', tagMeta$tag_no, tag, 'UKFSST_BestFitModel', SSTres, finVar, '.rds', sep='_'))
#fit2csv(fit = finMod, name = paste('FinishedTags_SPC_WC/BET', tagMeta$tag_no, tag, 'UKFSST_BestFitModel', SSTres, finVar, '', sep='_'))
#saveRDS(finMod, paste('FinishedTags_SPC_WC/BET', tagMeta$tag_no, tag, 'UKFSST_BestFitModel', SSTres, finVar, '.rds', sep='_'))

# Plot the ukdsst track and the LAT Viewer track on same figure
cbind(finMod$date, finMod$most.prob.track, DAL=finMod$days.at.liberty) %>% 
  ggplot() + borders('world2', fill='gray') +
  geom_point(data=finMod$nominal.track, aes(x,y), shape='o') +
  geom_path(aes(x,y,color=month), linewidth=1) + 
  geom_point(aes(x,y,color=month), size=2) +
  geom_point(data=tagData[c(1,nrow(tagData)),], aes(Long, Lat), size=5, shape='square') +
  coord_quickmap(xlim = c(min(tagData$Long, na.rm=T)-5, max(tagData$Long, na.rm=T)+5), ylim = c(min(tagData$Lat, na.rm=T)-5, max(tagData$Lat, na.rm=T)+5)) + 
  scale_color_viridis_c(option='turbo')

# Turn model ouput into standard format to pass on to SDM team
final <- cbind(finMod$most.prob.track, finMod$date, finMod$SST[,'o'])
#final$tag.serial <- ifelse(is.na(tagMeta$TagSerial), tag, tagMeta$TagSerial)
final$tag.serial <- ifelse(is.na(tagMeta$tag_no), tag, tagMeta$tag_no)
colnames(final)[c(1,2,6)] <- c('lon', 'lat', 'sst')
final <- final[,c('tag.serial', 'year', 'month', 'day', 'lon', 'lat', 'sst')]
final
write.csv(final, paste('../FinalOutputTags/BET', tagMeta$tag_no, tag, 'UKFSSTout', SSTres, finVar, '.csv', sep='_'))

