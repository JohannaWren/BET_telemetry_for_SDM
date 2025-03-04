title: "UKFSST Processing Script"
author: "Molly Scott"
date: "31/05/2023"
notes: "Johanna made changes to streamline reading in tag data and read some metadata informatin so we can 
fill the release and pop-off sites, and added Mitch's version of ukfsst script below. Johanna also made this 
script to work with Dave and Kim's tags which were not recovered thus don't have the out-Archive files."

#-------------------------------------------------------------------------------
# Read libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(ukfsst)
library(date)
library(kftrack)
library(suncalc)
library(ggplot2)


rm(list=ls())
# Set working directory
mainDir <- 'C:\\Users\\Johanna.Wren/Desktop/Tags_KH_DI/'
setwd(mainDir)

#-------------------------------------------------------------------------------
################## Format and clean DAP data for UKFSST ########################
#-------------------------------------------------------------------------------
# Set tag PTT number
tag <- '99894'

#-------------------------------------------------------------------------------
meta <- read.csv('../Tuna_metadata - ALL BET and YFT metadata.csv', na.strings=c(""," ","NA"))
#  Pull out the metadata record for your tag
tagMeta <- meta %>%
  filter(TagPTT == tag)
# Set release and pop-off locations
tagMeta$DeployDate <- as.Date(ifelse(is.na(tagMeta$Tagging.DeployDateUTC),
                                     tagMeta$Tagging.DeployDateLocal,
                                     tagMeta$Tagging.DeployDateUTC),
                                     format='%d-%b-%Y')
tagMeta$RecoverDate <- as.Date(tagMeta$PopoffDate, format='%d-%b-%Y')
tagMeta$tagTZ <-lutz::tz_lookup_coords(lat = tagMeta$Tagging.DeployLatitude, lon = tagMeta$Longitude...180., method = 'accurate')
tagMeta
# Print out tagging and recapture data for easy DAP access
matrix(tagMeta[,c('DeployDate','Longitude...180.', 'Tagging.DeployLatitude', 'RecoverDate', 'Recovery.PopoffLong', 'Recovery.PopoffLat')],nrow = 2, byrow = T)

# Read in tag and metadata
tagFiles <- list.files(path=paste0('ExtractedTags/', tag), full.names = T)
tagFiles
tagLoc1 <- read.csv(tagFiles[grep('Locations', tagFiles)], na.strings=c(""," ","NA"))
head(tagLoc1)
tagSST1 <- read.csv(tagFiles[grep('LightLoc', tagFiles)], na.strings=c(""," ","NA"))
head(tagSST1)
tagEnv <- read.csv(tagFiles[grep('Series.csv', tagFiles)], na.strings=c(""," ","NA"))
head(tagEnv)

# convert dates to appropriate formats
tagLoc1$YMD <- as.Date(tagLoc1$Date, format='%H:%M:%S %d-%b-%Y')
tagLoc1$YMDHMS <- as.POSIXct(tagLoc1$Date, format='%H:%M:%S %d-%b-%Y')
tagSST1$YMD <- as.Date(tagSST1$Day, format='%d-%b-%Y')
tagEnv$Date <- as.Date(tagEnv$Day, format='%d-%b-%Y')
tagEnv$DT_GMT <- as.POSIXct(paste(tagEnv$Date, tagEnv$Time), format='%Y-%m-%d %H:%M:%S', tz='GMT')
tagEnv$DT_local <- with_tz(tagEnv$DT_GMT, tagMeta$tagTZ)
tagEnv$Date_local <- as.Date(tagEnv$DT_local, tz='HST')

# Quickly plot the environmental data to make sure the input looks right
ggplot(tagEnv, aes(DT_GMT, Depth, color=Temperature)) + 
  geom_path() + 
  scale_y_reverse() + 
  scale_color_viridis_c(option = 'turbo')

#-------------------------------------------------------------------------------
sun <- getSunlightTimes(date = unique(tagEnv$Date_local),
                        lon=tagMeta$Longitude...180.,
                        lat=tagMeta$Tagging.DeployLatitude,
                        tz=tagMeta$tagTZ,
                        keep=c('sunrise', 'sunset', 'dawn', 'dusk', 'solarNoon')) %>%
  mutate(Sunrise_less1=sunrise-3600, Sunset_plus1=sunset+3600)

# Filter out night time only, using 1h after sunset and 1h before sunrise at the limits.
# I'm doing this in a loop because I couldn't figure out a straight forward way to do it with the changing sunrise and sunset time
tagNight <- list()
for (i in seq_along(unique(tagEnv$Date))) {
  rise <- sun %>% filter(date == unique(tagEnv$Date_local)[i]) %>% dplyr::select(Sunrise_less1)
  set <- sun %>% filter(date == unique(tagEnv$Date_local)[i]) %>% dplyr::select(Sunset_plus1)
  tagNight[[i]] <- tagEnv %>% 
    filter(Date_local == unique(tagEnv$Date_local)[i]) %>% 
    filter(DT_local < rise | DT_local > set)
}
# Because one calendar day has two half nights, I'm adding 12h so that all nighttime hours falls on the same calendar day to make it easier to calculate the mean. Then we'll subtract the 12h from the date to get back to the correct day. 
tagNightAllDepth <- data.table::rbindlist(tagNight) %>%
  mutate(DT_night = DT_local+(60*60*12), Date_night = as.Date(ymd_hms(DT_night))) %>%
  data.frame()

# Check to make sure we are pulling out the correct data for nighttime
ggplot() +  
  geom_path(data=tagEnv[which(tagEnv$Date_local == unique(tagNight[[47]]$Date_local)),], aes(DT_local, Depth), color='black', lwd=2) + 
  geom_path(data=tagNight[[47]], aes(DT_local, Depth), color='red') + 
  scale_y_reverse() 

# Filter out only sea surface depths. We usually use the top 10 meters, but because of bigeye's diving behavior I'm starting at 10 meters and moving deeper in increments of 5 meters until each day
# has at least two temperature records. I'm looping this and saving the end depth so I can put it in the file name later
for (d in seq(10,100, by=5)) {
  # Pull out all temps shallower than d meters
  tagEnvShallow <- tagNightAllDepth %>%
    filter(Depth <= d)
  # Checking to see how many daily records have less than 10 temperature records
  nrecs <- tagEnvShallow %>%
    group_by(Date) %>%
    summarise(count=n()) %>%
    filter(count < 2) %>%
    nrow()
  ndays <- length(unique(tagEnvShallow$Date_night)) - length(unique(tagNightAllDepth$Date_night))
  print(nrecs)
  print(ndays)
  # Ifnrow()# If there are still days with less than two records, add five meters to the depth and repeat until all days have temps
  if (nrecs > 0 | ndays < 0) {
    next
  } else {
    print(d)
    break
  }
}

# Then add it all together and calculate the average temperature. 
# Since I added 12h to each date so that one night would fall into one date for ease of averaging, I'm assigning that mean temperature to the date of the start of the night. So the mean temp for the night between monday and tuesday, will be mondays mean temp. 
tagNightDF <- tagEnvShallow %>% 
  group_by(Date_night) %>% 
  summarize(sst=mean(Temperature)) %>% 
  mutate(YMD = Date_night-(1)) %>% 
  data.frame()

#-------------------------------------------------------------------------------
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
  add_row(YMD=tagMeta$DeployDate, Longitude=tagMeta$Longitude...180., Latitude=tagMeta$Tagging.DeployLatitude, .before = T) %>%
  filter(YMD <= tagMeta$RecoverDate) %>% 
  arrange(YMD) %>% 
  data.frame()
head(tagLoc)

# Check to see if you need to add the last day or just change location
data.frame(Date=tagMeta$DeployDate, Lon=tagMeta$Longitude...180., Lat=tagMeta$Tagging.DeployLatitude)
head(tagLoc)
tail(tagLoc)
data.frame(Date=tagMeta$RecoverDate, Lon=tagMeta$Recovery.PopoffLong, Lat=tagMeta$Recovery.PopoffLat)
##
##
# DO ONE OR THE OTHER HERE, OR NOTHING
# If tag released instantly add pop-off location
tagLoc[nrow(tagLoc),c('Longitude', 'Latitude')] <- as.numeric(tagMeta[,c('Recovery.PopoffLong', 'Recovery.PopoffLat')])
tail(tagLoc)
# If tag bobbed for a while, remove everything after the fate date and set fix.last=F in UKFSST
tagLoc <- filter(tagLoc, YMD <= as.Date(tagMeta$Capture.FateDate, '%d-%b-%Y'))
#tagLoc <- filter(tagLoc, YMD <= as.Date(tagMeta$RecoverDate, '%d/%m/%Y'))
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
# # or if you just need to put the recovery locations on the last date/row
# tagLoc[nrow(tagLoc),'Longitude'] <- tagMeta$Rec_Lon
# tagLoc[nrow(tagLoc),'Latitude'] <- tagMeta$Rec_Lat
# tagLoc


#-------------------------------------------------------------------------------
# Merge locations and SST into one file with daily records
tagData <- tagLoc %>% 
  full_join(tagNightDF[,c('sst', 'YMD')]) %>% 
  mutate(day=day(YMD), month=month(YMD), year=year(YMD), Long=Longitude, Lat=Latitude, sst=sst) %>% 
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
tagData[which(tagData$Long == 0 & tagData$Lat == -80),4:5] <- NA
# # Remove all records with more than one NA
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

# # Remove any points that are way off. This will usually be the latitudes, the longitudes tend to be much better.
# # If you have a 'bad' point, just remove the lat but leave the longitude in there.
# # Trying to come up with a less subjective way to clean the data so I wrote some code that removes points that are farther away
# # than a bigeye can reasonable swim in the time between points. Doing this for both long and lats
# # Distance calculations
# library(geosphere)
# library(scales)
# BETswimSpeed <- 2.5  # in m/s
# times <- as.Date(do.call(sprintf, c(tagData[,3:1], '%s-%s-%s')))
# timeScale <-  rescale(times, to=c(0,(times[length(times)]-times[1])))
# 
# for (i in seq_along(timeScale[-1])) {
#   j=i+1
#   if (is.na(tagData$Lat[i])) {
#     i=tail(which(!is.na(tagData$Lat[1:i])),n=1)
#   }
#   dist <- distMeeus(data.frame(Long=190,Lat=tagData[c(i,j),5]))
#   totdist <- (dist > (BETswimSpeed*86400*diff.Date(times[c(i,j)])))
#   if (totdist == T) { tagData$Lat[j] <- NA}
# }
# 
# for (i in seq_along(timeScale[-1])) {
#   j=i+1
#   if (is.na(tagData$Long[i])) {
#     i=tail(which(!is.na(tagData$Long[1:i])),n=1)
#   }
#   dist <- distMeeus(data.frame(Long=tagData[c(i,j),4], Lat=25))
#   totdist <- (dist > (BETswimSpeed*86400*diff.Date(times[c(i,j)])))
#   if (totdist == T) { tagData$Long[j] <- NA}
# }

# See how it looks
tagData %>% 
  mutate(date=as.Date(paste(year, month, day, sep='-'))) %>% 
  ggplot(aes(date, Lat)) +
  geom_path(linewidth=0.5) +
  geom_point(size=2)

tagData %>% 
  mutate(date=as.Date(paste(year, month, day, sep='-'))) %>% 
  ggplot(aes(date, Long)) +
  geom_path(linewidth=0.5) +
  geom_point(size=2)

tagData %>% 
  mutate(date=as.Date(paste(year, month, day, sep='-'))) %>% 
  ggplot(aes(date, sst)) +
  geom_path(linewidth=0.5) +
  geom_point(size=2)


#-------------------------------------------------------------------------------
# Clean up records for each tag

# # Tag 168579
# tagData$Lat[which(tagData$Lat < 0)] <- NA


# # Tag 168578
# tagData$Lat[which(tagData$Lat < 10 | tagData$Lat >= 34)] <- NA
# tagData$Lat[24] <- NA
# tagData$Long[which(tagData$Long < 224.5)] <- NA

# # Tag 168573
# tagData$Lat[which(tagData$Lat < 22)] <- NA

# # Tag 142391
# tagData$Lat[which(tagData$Lat < 20)] <- NA
# tagData$Lat[c(61,69,73)] <- NA
# tagData$Long[c(29:35,38)] <- NA

# # Tag 142390
# tagData$Lat[which(tagData$Lat > 30)] <- NA
# tagData$Long[which(tagData$Long > 199)] <- NA

# # Tag 142388
# tagData$Lat[which(tagData$Lat < 15)] <- NA

# # Tag 142385
# # End tag at Fate Date
# fateDate <- as.Date(tagMeta$Capture.FateDate, '%d-%b-%Y')
# tagData <- tagData[1:which(tagData$day == day(fateDate) & tagData$month == month(fateDate)),]
# tagData$Lat[which(tagData$Lat > 35 | tagData$Lat <= 10)] <- NA

# # Tag 99897
# tagData$Lat[which(tagData$Lat < 15 )] <- NA

# Tag 99894
tagData$Lat[which(tagData$Lat > 24 | tagData$Lat < 12)] <- NA
tagData$Long[which(tagData$Long > 205)] <- NA


# # Tag 1890036
# tagData$Lat[which(tagData$Lat > 20 | tagData$Lat < -0)] <- NA
# tagData$Lat[c(23,212,214,367,375,382)] <- NA
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
write.csv(tagData, paste('FinishedTags/BET', tagMeta$TagSerial, tag, 'DateLonLatSST', paste0(d,'m'), 'cleaned.csv', sep='_'), row.names = F, quote = F)
#write.csv(tagData, paste('FinishedTags/BET', tagMeta$TagSerial, tag, 'DateLonLatSST', 'dawn', 'cleaned.csv', sep='_'), row.names = F, quote = F)



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
sst.path <- suppressWarnings(get.sst.from.server.jlkw(tagData, res='high'))

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


# ### Did the model converge? Is the fit good?
# ### Are there any spurious data points that need to be removed?
# 
# 
# ### Here, I turn off by.a to see if model performs better
# fit.2 <- kfsst(tagData,
#                fix.last = F,
#                u.a = T, 
#                v.a = T, 
#                D.a = T, 
#                bx.a = T, 
#                by.a = T, 
#                bsst.a = F,
#                sx.a = T,
#                sy.a = T,
#                ssst.a = T)
# 
# ### See if model converged
# print.kfsst(fit.2)
# ### Is the f### Is the f### Is the fit good?
# plot.kfsst(fit.2, ci=T)
# 
# 
# # which model has best negative log likelihood?
# fit.1$nlogL
# fit.2$nlogL
# 
# ### Did the model converge? Is the fit good?
# ### Are there any spurious data points that need to be removed?
# 
# 
# ### Here, I turn by.a back ON and assign starting parameters to see if model 
# ### performs better
# fit.3 <- kfsst(tagData,
#                fix.last = T,
#                u.a = T, 
#                v.a = T, 
#                D.a = T, 
#                bx.a = F, 
#                by.a = T, 
#                bsst.a = F,
#                sx.a = T,
#                sy.a = T,
#                ssst.a = T,
#                u.i = -3.240019  , 
#                v.i = 2.545434  , 
#                D.i = 1523.694  ,
#                bx.i = -3.280755  , 
#                by.i = -0.6426635        ,
#                bsst.i = 0,
#                sx.i = 2.057993  , 
#                sy.i = 3.398656  , 
#                ssst.i = 0.3656373        , 
#                a0.i = 1.907377e-09 , 
#                b0.i = 27.6435)
# 
# ### See if model converged
# print.kfsst(fit.3)
# ### Is the fit good?
# plot.kfsst(fit.3, ci=T)
# 
# 

#-------------------------------------------------------------------------------
############################ Export csv file ###################################
#-------------------------------------------------------------------------------
### Now, let's format our model into the csv source files
setwd(mainDir)
SSTres <- 'high'
finMod <- fit.1
finVar <- 'fit1'
fit2csv(fit = finMod, name = paste('FinishedTags/BET', tagMeta$TagSerial, tag, 'UKFSST_BestFitModel', SSTres, finVar, '', sep='_'))
saveRDS(finMod, paste('FinishedTags/BET', tagMeta$TagSerial, tag, 'UKFSST_BestFitModel', SSTres, finVar, 'surface.rds', sep='_'))

# Plot the ukdsst track and the LAT Viewer track on same figure
cbind(finMod$date, finMod$most.prob.track, DAL=finMod$days.at.liberty) %>%
  ggplot() + borders('world2', fill='gray') +
  geom_point(data=finMod$nominal.track, aes(x,y), shape='o') +
  geom_path(aes(x,y,color=month), linewidth=1) +
  geom_point(aes(x,y,color=month), size=2) +
  #geom_point(data=tagFinal[c(1,nrow(tagFinal)),], aes(lon, lat), size=5, shape='square') +
  coord_quickmap(xlim = c(min(finMod$most.prob.track$x, na.rm=T)-5, max(finMod$most.prob.track$x, na.rm=T)+5), ylim = c(min(finMod$most.prob.track$y, na.rm=T)-5, max(finMod$most.prob.track$y, na.rm=T)+5)) +
  scale_color_viridis_c(option='turbo') +
  geom_path(data=myDat, aes(Most.Likely.Longitude+360, Most.Likely.Latitude), color='red')

myDat <- read.csv('../KH_DI_WCdownloads/168577_WCdownload/18P1185-168577-2-GPE3.csv', skip=5)
