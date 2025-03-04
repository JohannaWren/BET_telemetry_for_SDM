#-------------------------------------------------------------------------------
# Date: July 29, 2024
# Author: Johanna Wren
# Email: johanna.wren@noaa.gov
# Description: Processing data output from Wildlife Computers DAP for use in UKFSST location processing. 
#              This script calculates average daily nighttime temperature from the out-Archive file that
#              we get from the WC portal. I'm using the same code as when processing Lotek tags in an
#              attempt to keep the process as consistent as possible, but getting the sunset and sunrise
#              times from the suncalc packages since DAP doesn't provide those times, only dawn and dusk.
#              This code and method is based off Mitch Lovell's 
#              "Location Processing Protocol_for Lotek.docx" and "Processing Wildlife Computer Mk9s.docx" 
#              protocol and translated into R. 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Clear workspace
rm(list=ls())

# Set working directory
mainDir <- "C:/Users/Johanna.Wren/Desktop/SPC_WC/"
setwd(mainDir)

#-------------------------------------------------------------------------------
# Load Libraries
library(dplyr)
library(chron)
library(lubridate)
library(suncalc)
library(ggplot2)

#-------------------------------------------------------------------------------
# SET TAG NUMBER (Ptt)
#-------------------------------------------------------------------------------
# This is generally the Ptt number, but it's up to you. Just make sure that it matches what you are searching
# for in the next section when you read in the data. 
tag <- '1990092'
tag2 <- tag

#-------------------------------------------------------------------------------
# Load Data
#-------------------------------------------------------------------------------
# Tag metadata
#meta <- read.csv('../BET_metadata_SPC.csv', na.strings=c(""," ","NA"))
#meta <- read.csv('../Updated Archival Tag Data 2024.csv', na.strings=c(""," ","NA"))
meta <- read.csv('../WLC_Tags_fromJoeJan2023.csv', na.strings=c(""," ","NA"))
#meta <- read.csv('../ArchivalTag_1MonthPlus_Summary.csv', na.strings=c(""," ","NA"))
#meta <- read.csv('../Tuna_metadata - ALL BET and YFT metadata.csv', na.strings=c(""," ","NA"))
#  Pull out the metadata record for your tag
tagMeta <- meta %>% 
  #filter(TagSerial == tag)
  filter(Arc_tag_no == tag)
# Make time zone record based on tagging lon/lat becuase I don't understand SPC's time zones
tagMeta$tagTZ <-lutz::tz_lookup_coords(lat = tagMeta$Rel_Lat, lon = tagMeta$Rel_Lon-360, method = 'accurate')
#tagMeta$tagTZ <-lutz::tz_lookup_coords(lat = tagMeta$release_lat, lon = tagMeta$release_lon, method = 'accurate')
#tagMeta$tagTZ <-lutz::tz_lookup_coords(lat = tagMeta$Tagging.DeployLatitude, lon = tagMeta$Tagging.DeployLongitude_180, method = 'accurate')
tagMeta <- tagMeta %>%
  mutate(DeployDT=as.POSIXct(event_date, format='%Y-%m-%d', tz=tagTZ),
         DeployDate=date(DeployDT),
         RecoverDT=as.POSIXct(best_catchdate, format='%Y-%m-%d', tz=tagTZ),
         RecoverDate=date(RecoverDT))
# tagMeta <- tagMeta %>%
#   mutate(DeployDT=as.POSIXct(DeployDate, format='%d/%m/%Y %H:%M', tz=tagTZ),
#          DeployDate=date(DeployDT),
#          RecoverDT=as.POSIXct(RecoverDate, format='%d/%m/%Y %H:%M', tz=tagTZ),
#          RecoverDate=date(RecoverDT))
# tagMeta <- tagMeta %>% 
#   mutate(DeployDT=as.POSIXct(Tagging.DeployDateLocal, format='%d/%m/%Y %H:%M', tz=tagTZ), 
#          DeployDate=date(DeployDT), 
#          RecoverDT=as.POSIXct(Capture.FateDate, format='%d/%m/%Y %H:%M', tz=tagTZ), 
#          RecoverDate=date(RecoverDT))

# Read in DAP Files
tagFiles <- list.files(path=paste0('ExtractedTags_SPC_WC/',tag), full.names = T)
tagFiles
# Locations file from DAP
tagDat <- read.csv(tagFiles[grep('Locations', tagFiles)], na.strings=c(""," ","NA"))
# LightLoc file from DAP (this is only if you don't have the Archive file from the WC file)
#tagTemp <- read.csv(tagFiles[grep('LightLoc', tagFiles)])
# Archive file from WC portal
tagArc <- read.csv(list.files(path=paste0('ExtractedTags_SPC_WC/', tag, '/', tag2, '_WC_out'), pattern='out-Archive', full.names = T))

#-------------------------------------------------------------------------------
# Process data
#-------------------------------------------------------------------------------
# Change some data formats and limit the range of the data to tagging start and end dates
# Change date to date format
tagDat$DT <- as.POSIXct(tagDat$Date, format='%H:%M:%S %d-%b-%Y', tz='UTC')
tagDat$DT_local <- with_tz(tagDat$DT, tagMeta$tagTZ)
tagArc$DT <- as.POSIXct(tagArc$Time, format='%H:%M:%S %d-%b-%Y', tz='UTC')
tagArc$DT_local <- with_tz(tagArc$DT, tagMeta$tagTZ)

#tagTemp$YMD <- as.Date(tagTemp$Day, format='%d-%b-%Y')
# #tagDat$Date <- as.Date(tagDat$dd.mm.yy, format='%d/%m/%y')
# tagTemp$TimeS <- as.POSIXct(tagTemp$TimeS, format='%H:%M:%S %d/%m/%y', tz='UTC')
# tagTemp$DT_local <- with_tz(tagTemp$TimeS, tagMeta$tagTZ)
# tagPres$TimeS <- as.POSIXct(tagPres$TimeS, format='%H:%M:%S %d/%m/%y', tz='UTC')
# tagPres$DT_local <- with_tz(tagPres$TimeS, tagMeta$tagTZ)

# Calculate sunrise and sunset times using tagging location and local TZ
sun <- getSunlightTimes(date = unique(as.Date(tagArc$DT_local)), 
                        lon=tagMeta$Rel_Lon, 
                        lat=tagMeta$Rel_Lat, 
                        tz=tagMeta$tagTZ,
                        keep=c('sunrise', 'sunset', 'dawn', 'dusk', 'solarNoon')) %>% 
                        mutate(Sunrise_less1=sunrise-3600, Sunset_plus1=sunset+3600)
# sun <- getSunlightTimes(date = unique(as.Date(tagArc$DT_local)), 
#                         lon=tagMeta$Tagging.DeployLongitude, 
#                         lat=tagMeta$Tagging.DeployLatitude, 
#                         tz=tagMeta$tagTZ,
#                         keep=c('sunrise', 'sunset', 'dawn', 'dusk', 'solarNoon')) %>% 
#   mutate(Sunrise_less1=sunrise-3600, Sunset_plus1=sunset+3600)

# Remove all dates before and after the tag date
tagDat <- tagDat %>% 
  filter(DT_local >= tagMeta$DeployDate & DT_local <= tagMeta$RecoverDate) %>% 
  dplyr::select(Date, Latitude, Longitude, DT, DT_local)

# Merge temperature and pressure data from tag and remove points outside the tag dates. 
# Make a time column and a date column
tagEnv <- tagArc %>% 
  dplyr::select(DT, DT_local, Stalk.Temp, Depth) %>% 
  filter(DT_local >= tagMeta$DeployDate & DT_local <= tagMeta$RecoverDate) %>% 
  mutate(Date = as.Date(format(DT_local, '%Y-%m-%d')), 
         TimeHM = format(DT_local, '%H:%M'), 
         TimeHMS=times(format(DT_local, '%H:%M:%S')))

# Quickly plot the environmental data to make sure the input looks right
ggplot(tagEnv, aes(DT_local, Depth, color=Stalk.Temp)) + 
  geom_path() + 
  scale_y_reverse() + 
  scale_color_viridis_c(option = 'turbo')



#-------------------------------------------------------------------------------
# Filter out night time only, using 1h after sunset and 1h before sunrise at the limits.
# I'm doing this in a loop because I couldn't figure out a straight forward way to do it with the changing sunrise and sunset time
tagNight <- list()
for (i in seq_along(unique(tagEnv$Date))) {
  rise <- sun %>% filter(date == unique(tagEnv$Date)[i]) %>% dplyr::select(Sunrise_less1)
  set <- sun %>% filter(date == unique(tagEnv$Date)[i]) %>% dplyr::select(Sunset_plus1)
  tagNight[[i]] <- tagEnv %>% 
    filter(Date == unique(tagEnv$Date)[i]) %>% 
    filter(DT_local < rise | DT_local > set)
}
# Because one calendar day has two half nights, I'm adding 12h so that all nighttime hours falls on the same calendar day to make it easier to calculate the mean. Then we'll subtract the 12h from the date to get back to the correct day. 
tagNightAllDepth <- data.table::rbindlist(tagNight) %>%
  mutate(DT_night = DT_local+(60*60*12), Date_night = as.Date(ymd_hms(DT_night))) %>%
  data.frame()

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
  summarize(sst=mean(Stalk.Temp)) %>% 
  mutate(Date = Date_night-(1)) %>% 
  data.frame()

#-------------------------------------------------------------------------------
# Join the average night time time from above with Lon and Lat from the Locations file
# Separate dates into day, month, year
tagFinal <- tagDat %>% 
  dplyr::select(-Date) %>% 
  mutate(Date=as.Date(DT)) %>%
  group_by(Date) %>% 
  filter(DT_local == min(DT)) %>% 
  distinct() %>% 
  ungroup() %>% 
  rename(lon=Longitude, lat=Latitude) %>% 
  dplyr::select(Date, lon, lat) %>%
  left_join(tagNightDF) %>% 
  mutate(day=day(Date), month=month(Date), year=year(Date)) %>% 
  dplyr::select(day, month, year, lon, lat, sst) %>% 
  data.frame()

# Make sure release and recovery information is included in the dataset and accurate. This may take on many different flavors, this is just one to fix it
# Deploy and recover info
data.frame(Date=c(tagMeta$DeployDate, tagMeta$RecoverDate), Lon=c(tagMeta$Rel_Lon, tagMeta$Rec_Lon), Lat=c(tagMeta$Rel_Lat, tagMeta$Rec_Lat))
head(tagFinal)
tail(tagFinal)
# Replace the release and capture locations with the known release and recapture locations
tagFinal <- tagFinal %>% 
  add_row(day=day(tagMeta$DeployDate), month=month(tagMeta$DeployDate), year=year(tagMeta$DeployDate), lon=tagMeta$Rel_Lon, lat=tagMeta$Rel_Lat, .before=T) %>%  #lon=tagMeta$Tagging.DeployLongitude_180, lat=tagMeta$Tagging.DeployLatitude, .before = T) %>% 
  add_row(day=day(tagMeta$RecoverDate), month=month(tagMeta$RecoverDate), year=year(tagMeta$RecoverDate), lon=tagMeta$Rec_Lon, lat=tagMeta$Rec_Lat)
#tagFinal[nrow(tagFinal),c('lon', 'lat')] <- as.numeric(tagMeta[,c('RecoveryBestLon.180', 'RecoveryBestLat')])
#tagFinal[1,c('lon', 'lat')] <- tagMeta[,c('Tagging.DeployLongitude', 'Tagging.DeployLatitude')]
# Make sure you don't have artifacts from joining at the end of your dataframe. If you do, remove them
#tail(tagFinal)
#tagFinal <- tagFinal[-nrow(tagFinal),]
# This is to remove half days of data that may be wrong
tagFinal$sst[nrow(tagFinal)] <- NA 

# And finally change all the Lons to 360 degree
tagFinal$lon <- ifelse(tagFinal$lon < 0, tagFinal$lon+360, tagFinal$lon)

#-------------------------------------------------------------------------------
# Plot the track so you can see wrong points
ggplot(tagFinal, aes(lon, lat, color=sst)) + 
  borders('world2', fill='gray') +
  geom_path() + 
  geom_point() +
  geom_point(data=tagFinal[c(1,nrow(tagFinal)),], aes(lon, lat), size=3) +
  coord_quickmap(xlim = c(min(tagFinal$lon)-5, max(tagFinal$lon)+5), ylim = c(min(tagFinal$lat)-5, max(tagFinal$lat)+5)) + 
  scale_color_viridis_c(option='turbo')

# Remove any points that are way off. This will usually be the latitudes, the longitudes tend to be much better.
# If you have a 'bad' point, just remove the lat but leave the longitude in there. 
tagFinal %>% 
  mutate(date=as.Date(paste(year, month, day, sep='-'))) %>% 
  ggplot(aes(date, lat)) +
    geom_path(linewidth=0.5) +
    geom_point(size=2)

tagFinal %>% 
  mutate(date=as.Date(paste(year, month, day, sep='-'))) %>% 
  ggplot(aes(date, lon)) +
  geom_path(linewidth=0.5) +
  geom_point(size=2)

tagFinal %>% 
  mutate(date=as.Date(paste(year, month, day, sep='-'))) %>% 
  ggplot(aes(date, sst)) +
  geom_path(linewidth=0.5) +
  geom_point(size=2)

#-------------------------------------------------------------------------------
# Clean up records for each tag

# # Tag 1890036
# tagFinal$lat[which(tagFinal$lat > 20 | tagFinal$lat < -0)] <- NA
# tagFinal$lat[c(23,212,214,367,375,382)] <- NA

# # Tag 1890056
# tagFinal$lat[which(tagFinal$lat > 10 | tagFinal$lat < -5)] <- NA
# tagFinal$lon[c(15:16,20,29,30,85)] <- NA

# # Tag 168581
# tagFinal$lat[which(tagFinal$lat > 10 | tagFinal$lat < -7)] <- NA
# tagFinal$lon[which(tagFinal$lon < 187)] <- NA
# tagFinal$lon[c(5,6,10,23,115,156)] <- NA

# Tag 168585
#tagFinal <- tagFinal[which(tagFinal$lon >= 220),]
#tagFinal$lat[which(tagFinal$lat <= 20)] <- NA

# Tag 168587
# tagFinal$lat[which(tagFinal$lat < 15)] <- NA
# tagFinal$lat[which(tagFinal$lat > 50)] <- NA
# tagFinal$lon[which(tagFinal$lon > 236)] <- NA
# tagFinal$lon[50] <- NA
# tagFinal$lon[44] <- NA

# Tag 0990298
# tagFinal$lat[which(tagFinal$lat <= -25 | tagFinal$lat > 30)] <- NA
# tagFinal$lat[166] <- NA
# tagFinal$lon[which(tagFinal$lon < 210)] <- NA
# tagFinal$sst[1] <- NA

# # Tag 0890209
# tagFinal$lat[which(tagFinal$lat > 10 | tagFinal$lat < -10)] <- NA
# tagFinal$lon[which(tagFinal$lon > 147)] <- NA
# tagFinal$lon[c(136,106)] <- NA
# tagFinal$sst[which(tagFinal$sst < 28.6)] <- NA

# Tag 0890010
# tagFinal$lat[which(tagFinal$lat > 10 | tagFinal$lat < -10)] <- NA
# tagFinal$sst[nrow(tagFinal)] <- NA

# # Tag 1290657
# tagFinal$lat[which(tagFinal$lat < -25 | tagFinal$lat > 30)] <- NA
# tagFinal$lon[which(tagFinal$lon > 210 | tagFinal$lon < 150)] <- NA
# tagFinal$lon[c(118,116,111,98)] <- NA
# tagFinal$sst[which(tagFinal$sst < 26.5)] <- NA

# # Tag 1290658
# tagFinal <- tagFinal[1:33,]   # remove the last two days of tag information because the fish was caught before the metadata recapture date

# # Tag 1290659
# tagFinal$lat[which(tagFinal$lat > 10 | tagFinal$lat < -10)] <- NA
# tagFinal$lon[which(tagFinal$lon < 150)] <- NA

# # Tag 1290664
# tagFinal$lon[22] <- NA
# tagFinal$lat[which(tagFinal$lat < -6)] <- NA

# # Tag 1390183
# tagFinal$lat[which(tagFinal$lat > 5 | tagFinal$lat <= -5)] <- NA
# tagFinal$lon[which(tagFinal$lon > 190.5)] <- NA

# Tag 14290659
# tagFinal$lat[which(tagFinal$lat > 15)] <- NA
# tagFinal$sst[nrow(tagFinal)] <- NA

# Tag 189081
# tagFinal$lat[which(tagFinal$lat > 12)] <- NA
# tagFinal$lat[which(tagFinal$lat < -10)] <- NA
# tagFinal$lon[which(tagFinal$lon < 188)] <- NA
# tagFinal[115,'lon'] <- NA
# tagFinal[5:6,'lon'] <- NA

# # Tag 1090387
# tagFinal$lat[which(tagFinal$lat > 20 | tagFinal$lat < -25)] <- NA
# tagFinal[c(107,362,379),'lat'] <- NA
# tagFinal[158,'lon'] <- NA
# tagFinal[c(207:214,245,247,327,328),'sst'] <- NA

# # Tag 1190070
# tagFinal$sst[nrow(tagFinal)-1] <- NA
# tagFinal$lon[which(tagFinal$lon > 191)] <- NA
# tagFinal$lat[which(tagFinal$lat > 1 | tagFinal$lat < -6)] <- NA

# # Tag 1490057
# tagFinal <- tagFinal[1:117,]
# tagFinal$lon[which(tagFinal$lon < 195)] <- NA
# tagFinal$lat[which(tagFinal$lat > 25 | tagFinal$lat < -10)] <- NA

# # Tag 1490061
# tagFinal$lat[which(tagFinal$lat > 25 | tagFinal$lat < - 13)] <- NA
# tagFinal$lon[c(10,11,19)] <- NA

# # Tag 1490065
# tagFinal$lat[which(tagFinal$lat > 3.4 | tagFinal$lat < -8)] <- NA
# tagFinal$lat[c(49,50,85,87,88,98,106,107,144,145,151,177,186,195,126,135,196,191,192,185,176)] <- NA
# tagFinal$sst[c(133,135,188,191,193)] <- NA
# tagFinal$lon[c(35,48,126,163)] <- NA

# # Tag 1590052
# tagFinal$lat[which(tagFinal$lat > 8 | tagFinal$lat < -11.5)] <- NA
# tagFinal$lat[c(6:8,11,14,106)] <- NA
# tagFinal$lon[c(11:14,27,106)] <- NA

# # tag 1690100
# tagFinal$lat[which(tagFinal$lat > 0)] <- NA
# tagFinal$lon[29] <- NA

# Tag 1690142
# tagFinal$lat[which(tagFinal$lat > 0)] <- NA
# tagFinal$lat[which(tagFinal$lat < -15)] <- NA

# # Tag 1690171
# # Remove all points outside of daily swimming distance
# library(geosphere)
# library(scales)
# BETswimSpeed <- 3 # in m/s
# times <- as.Date(do.call(sprintf, c(tagFinal[,3:1],'%s-%s-%s')))
# timeScale <- rescale(times, to=c(0,(times[length(times)])))
# # Loop through each point and compare distance to previous point. If further away than a tuna can swin in that time, the point is set to NA
# # This isn't perfect; it removes some points that should be kept and keeps others, but on a dataset like this, it is the best option I have found
# for (i in seq_along(timeScale[-1])) {
#   j=i+1
#   if (is.na(tagFinal$lat[i])) {
#     i=tail(which(!is.na(tagFinal$lat[1:i])),n=1)
#   }
#   dist <- distMeeus(data.frame(Long=160, Lat=tagFinal[c(i,j),5]))
#   totdist <- (dist > (BETswimSpeed*86400*as.numeric(diff.Date(times[c(i,j)]))))
#   if (totdist == T) { tagFinal$lat[j] <- NA }
# }
# # Clean up the rest manually
# tagFinal$lat[which(tagFinal$lat >= 0 | tagFinal$lat <= -15)] <- NA
# tagFinal$lon[which(tagFinal$lon < 155)] <- NA
# tagFinal$lon[c(4,16,18,46,73,77,80,96,88,100,117,119,120,128,129,133,175,182,194:196,203,204,210,221,226,273,281,283,285,318,323,335,352,360,385,387,427,437,459,468,482,516,517,528,529,531,532,537,538,541)] <- NA

# # Tag 1690174
# tagFinal$lat[which(tagFinal$lat > 0 | tagFinal$lat < -12)] <- NA
# tagFinal$lon[which(tagFinal$lon < 165)] <- NA

# # Tag 179005
# tagFinal$lat[which(tagFinal$lat > 0 | tagFinal$lat <= -15)] <- NA
# tagFinal$lon[which(tagFinal$lon < 160)] <- NA
# tagFinal$lon[c(6,33,67,68)] <- NA

# # Tag 1790072
# tagFinal$lat[which(tagFinal$lat < -16 | tagFinal$lat > 10)] <- NA
# tagFinal$lon[which(tagFinal$lon < 172.5)] <- NA
# tagFinal$lon[c(62)] <- NA

# # Tag 1790165
# tagFinal$lat[which(tagFinal$lat < -10 | tagFinal$lat > 30)] <- NA
# tagFinal$lon[which(tagFinal$lon > 181 | tagFinal$lon < 173)] <- NA

# # Tag 1790319
# tagFinal$lat[which(tagFinal$lat > 6 | tagFinal$lat < -13)] <- NA
# tagFinal$lat[c(73,74,100,116,118,120,130,149,151,157)] <- NA
# tagFinal$lon[c(53,73,94,116)] <- NA

# Tag 1990092
tagFinal <- tagFinal[-49,]  # remove duplicate final row
tagFinal$lat[which(tagFinal$lat > 10 | tagFinal$lat < -5)] <- NA
tagFinal$lat[c(2,15,18,25,27,31,37,41,46)] <- NA


# Remove all records with more than one NA
which(rowSums(is.na(tagFinal))>1)
tagFinal <- tagFinal[-which(rowSums(is.na(tagFinal))>1),] 
# Save the final tag file for use in UKFSST
# The file need: day month, year, lon, lat, sst
#write.csv(tagFinal, paste('FinishedTags_SPC_WC_new/BET', tagMeta$tag_no, tag, 'DateLonLatSST', paste0(d,'m'), 'cleaned.csv', sep='_'), row.names = F, quote = F)
write.csv(tagFinal, paste('FinishedTags_SPC_WC/BET', tagMeta$tag_no, tag, 'DateLonLatSST', paste0(d,'m'), 'cleaned.csv', sep='_'), row.names = F, quote = F)


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


# Check the final tag to make sure you have the start dates you should
head(tagFinal)
# And sometimes when you join above you get the records that can't join tacked on at the end. These are usually the first day of the tag
# so make sure you don't have a faulty record here because it messes up the get.sst.from.server function
tail(tagFinal)

### The sst.path is stored in the environment and used by kfsst() model below
### You only have to run this once if you save your Work Environment
sst.path <- suppressWarnings(get.sst.from.server.jlkw(na.omit(tagFinal), res='high'))


#-------------------------------------------------------------------------------
# Run the model
#-------------------------------------------------------------------------------
### Initial model - where bx.a and bsst.a = F
### We start here because the archival tags are really good at predicting
### longitude and sst; therefore, we don't want to bias these parameters
fit.1 <- kfsst(tagFinal,
                    fix.last = T,
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

# 
# ### Did the model converge? Is the fit good?
# ### Are there any spurious data points that need to be removed?
# 
# 
# ### Here, I turn off by.a to see if model performs better
# fit.2 <- kfsst(tagFinal,
#                       fix.last = T,
#                       u.a = T,
#                       v.a = T,
#                       D.a = T,
#                       bx.a = T,
#                       by.a = T,
#                       bsst.a = F,
#                       sx.a = T,
#                       sy.a = T,
#                       ssst.a = T)
# 
# ### See if model converged
# print.kfsst(fit.2)
# ### Is the fit good?
# plot.kfsst(fit.2, ci=T)
# 
# 
# ### Did the model converge? Is the fit good?
# ### Are there any spurious data points that need to be removed?
# 
# 
# ### Here, I turn by.a back ON and assign starting parameters to see if model 
# ### performs better
# fit.3 <- kfsst(tagFinal,
#                       fix.last = T,
#                       u.a = T, 
#                       v.a = T, 
#                       D.a = T, 
#                       bx.a = F, 
#                       by.a = T, 
#                       bsst.a = F,
#                       sx.a = T,,
#                       sy.a = T
#                       ssst.a = T,
#                       u.i = 3.665244, 
#                       v.i = 7.294314 , 
#                       D.i = 2233.109 ,
#                       bx.i = 3.348589 , 
#                       by.i = -0.92729    ,
#                       bsst.i = 0,
#                       sx.i = 2.068778 , 
#                       sy.i = 3.624964 , 
#                       ssst.i = 0.3294515    , 
#                       a0.i = 1.749781e-07, 
#                       b0.i = -16.0084)
# 
# ### See if model converged
# print.kfsst(fit.3)
# ### Is the fit good?
# plot.kfsst(fit.3, ci=T)



#-------------------------------------------------------------------------------
############################ Export csv file ###################################
#-------------------------------------------------------------------------------
### Now, let's format our model into the csv source files
setwd(mainDir)
SSTres <- 'high'
finMod <- fit.1
finVar <- 'fit1'
fit2csv(fit = finMod, name = paste('FinishedTags_SPC_WC/BET', tagMeta$tag_no, tag, 'UKFSST_BestFitModel', SSTres, finVar, '', sep='_'))
saveRDS(finMod, paste('FinishedTags_SPC_WC/BET', tagMeta$tag_no, tag, 'UKFSST_BestFitModel', SSTres, finVar, '.rds', sep='_'))

# Plot the ukdsst track and the LAT Viewer track on same figure
cbind(finMod$date, finMod$most.prob.track, DAL=finMod$days.at.liberty) %>%
  ggplot() + borders('world2', fill='gray') +
  geom_point(data=finMod$nominal.track, aes(x,y), shape='o') +
  geom_path(aes(x,y,color=month), linewidth=1) +
  geom_point(aes(x,y,color=month), size=2) +
  #geom_point(data=tagFinal[c(1,nrow(tagFinal)),], aes(lon, lat), size=5, shape='square') +
  coord_quickmap(xlim = c(min(finMod$most.prob.track$x, na.rm=T)-5, max(finMod$most.prob.track$x, na.rm=T)+5), ylim = c(min(finMod$most.prob.track$y, na.rm=T)-5, max(finMod$most.prob.track$y, na.rm=T)+5)) +
  scale_color_viridis_c(option='turbo')





########################################################
# Remove all points outside of daily swimming distance
#######################################################
library(geosphere)
library(scales)
BETswimSpeed <- 4
times <- as.Date(do.call(sprintf, c(tagFinal[,3:1],'%s-%s-%s')))
timeScale <- rescale(times, to=c(0,(times[length(times)])))

for (i in seq_along(timeScale[-1])) {
  j=i+1
  if (is.na(tagFinal$lat[i])) {
    i=tail(which(!is.na(tagFinal$lat[1:i])),n=1)
  }
  dist <- distMeeus(data.frame(Long=160, Lat=tagFinal[c(i,j),5]))
  totdist <- (dist > (BETswimSpeed*86400*as.numeric(diff.Date(times[c(i,j)]))))
  if (totdist == T) { tagFinal$lat[j] <- NA }
}



