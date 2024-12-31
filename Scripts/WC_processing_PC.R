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

#-------------------------------------------------------------------------------
# SET TAG NUMBER (Ptt)
#-------------------------------------------------------------------------------
# This is generally the Ptt number, but it's up to you. Just make sure that it matches what you are searching
# for in the next section when you read in the data. 
tag <- '1290657'

#-------------------------------------------------------------------------------
# Load Data
#-------------------------------------------------------------------------------
# Tag metadata
meta <- read.csv('../BET_metadata_SPC.csv', na.strings=c(""," ","NA"))
#  Pull out the metadata record for your tag
tagMeta <- meta %>% 
  filter(TagSerial == tag)
# Make time zone record based on tagging lon/lat becuase I don't understand SPC's time zones
tagMeta$tagTZ <-lutz::tz_lookup_coords(lat = tagMeta$Tagging.DeployLatitude, lon = tagMeta$Tagging.DeployLongitude_180, method = 'accurate')
tagMeta <- tagMeta %>% 
  mutate(DeployDT=as.POSIXct(Tagging.DeployDateLocal, format='%d/%m/%Y %H:%M', tz=tagTZ), 
         DeployDate=date(DeployDT), 
         RecoverDT=as.POSIXct(Capture.FateDate, format='%d/%m/%Y %H:%M', tz=tagTZ), 
         RecoverDate=date(RecoverDT))
# Read in DAP Files
tagFiles <- list.files(path=paste0('ExtractedTags_SPC_WC/',tag), full.names = T)
tagFiles
# Locations file from DAP
tagDat <- read.csv(tagFiles[grep('Locations', tagFiles)], na.strings=c(""," ","NA"))
# LightLoc file from DAP
#tagTemp <- read.csv(tagFiles[grep('LightLoc', tagFiles)])
# Archive file from WC portal
tagArc <- read.csv(list.files(path=paste0('ExtractedTags_SPC_WC/', tag, '_', tagMeta$TagPTT, '_WC_out'), pattern='out-Archive', full.names = T))

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
                        lon=tagMeta$Tagging.DeployLongitude, 
                        lat=tagMeta$Tagging.DeployLatitude, 
                        tz=tagMeta$tagTZ,
                        keep=c('sunrise', 'sunset', 'dawn', 'dusk', 'solarNoon')) %>% 
                        mutate(Sunrise_less1=sunrise-3600, Sunset_plus1=sunset+3600)

# # Change sunrise and sunset to time formats in UTC
# tagDat$SunriseDT <- tagDat$dd.mm.yy + hm(tagDat$Sunrise)
# tagDat$SunsetDT <- tagDat$dd.mm.yy + hm(tagDat$Sunset)
# tagDat$NoonDT <- tagDat$dd.mm.yy + hm(tagDat$Noon)
# tagDat$MidnightDT <- tagDat$dd.mm.yy + hm(tagDat$Midnight)
# # Then change that to local time for the tag
# tagDat$SunriseDTlocal <- with_tz(tagDat$SunriseDT, tz=tagMeta$tagTZ)
# tagDat$SunsetDTlocal <- with_tz(tagDat$SunsetDT, tz=tagMeta$tagTZ)
# tagDat$NoonDTlocal <- with_tz(tagDat$NoonDT, tz=tagMeta$tagTZ)

# # Subtract one hour from Sunrise (this value will be used to filter raw data when obtaining sst values)
# tagDat$Sunrise_less1 <- format(tagDat$SunriseDTlocal-3600, '%H:%M')
# # Add one hour to Sunset (this value will be used to filter raw data when obtaining sst values)
# tagDat$Sunset_plus1 <- format(tagDat$SunsetDTlocal+3600, '%H:%M')

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

#-------------------------------------------------------------------------------
# Limit the depths to top 10 meters ideally. This works well for YFT but may need to go deeper for BET.
tagEnvShallow <- tagEnv %>% 
  filter(Depth <= 40)
# Checking to see how many daily records have less than 10 temperature records
tagEnvShallow %>% group_by(Date) %>% summarise(count=n()) %>% filter(count < 10) %>% dim()
# How many days the tag is at large
tagEnvShallow$Date[nrow(tagEnvShallow)]-tagEnvShallow$Date[1]
# How many days have SST info
tagEnvShallow %>% group_by(Date) %>% summarise(count=n()) %>% nrow()
# If you have less days than days at large with SST info, deepen the criteria for SST some more until you have at least all days represented
# and not too many with only a few temp records

# Filter out night time only, using 1h after sunset and 1h before sunrise at the limits.
# I'm doing this in a loop because I couldn't figure out a straight forward way to do it with the changing sunrise and sunset time
tagNight <- list()
for (i in seq_along(unique(tagEnv$Date))) {
  rise <- sun %>% filter(date == unique(tagEnv$Date)[i]) %>% dplyr::select(Sunrise_less1)
  set <- sun %>% filter(date == unique(tagEnv$Date)[i]) %>% dplyr::select(Sunset_plus1)
  tagNight[[i]] <- tagEnvShallow %>% 
    filter(Date == unique(tagEnv$Date)[i]) %>% 
    filter(DT_local < rise | DT_local > set)
}
# Because one calendar day has two half nights, I'm adding 12h so that all nighttime hours falls on the same calendar day to make it easier to calculate the mean. Then we'll subtract the 12h from the date to get back to the correct day. 
# Then add it all together and calculate the average temperature. 
tagNightDF <- data.table::rbindlist(tagNight) %>% 
  mutate(DT_night = DT_local+(60*60*12), Date_night = as.Date(ymd_hms(DT_night))) %>% 
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
  dplyr::select(day, month, year, lon, lat, sst)

# Replace the release and capture locations with the known release and recapture locations
tagFinal <- tagFinal %>% 
  add_row(day=16, month=5, year=2008, lon=tagMeta$Tagging.DeployLongitude_180, lat=tagMeta$Tagging.DeployLatitude, .before = T) %>% 
  data.frame()
tagFinal[nrow(tagFinal),c('lon', 'lat')] <- as.numeric(tagMeta[,c('RecoveryBestLon.180', 'RecoveryBestLat')])
#tagFinal[1,c('lon', 'lat')] <- tagMeta[,c('Tagging.DeployLongitude', 'Tagging.DeployLatitude')]
# Make sure you don't have artifacts from joining at the end of your dataframe. If you do, remove them
tail(tagFinal)
#tagFinal <- tagFinal[-nrow(tagFinal),]
# This is to remove half days of data that may be wrong
tagFinal$sst[nrow(tagFinal)] <- NA 

#-------------------------------------------------------------------------------
# # Remove missing values intruduced by LatViewer Studio
# # LatViewer sometimes will add a lon=200 or lat=100 as missing values so we need to go through and give those slots an average value instead
# # Mitch has been using a mean of the 10 points before and after the missing value
# # Find out which records have missing values
# idxLon <- which(tagFinal$lon == 200)
# idxLat <- which(tagFinal$lat == 100)
# idxLon
# idxLat
# # Beause some missing values are next to eachother, we don't want them as part of the average value we put in, so I'm changing them to NAs here
# tagFinal$lat[idxLat] <- NA
# tagFinal$lon[idxLon] <- NA
# # Then start looping through the points with the missing value code and replace with the average of the nearest 20 points
# # I separated out lon and lat here in case only one has them missing values it's easier to not break the code. But I'm sure there is a much more elegant way of coding this
# # if (length(idxLat) > 0) {
# #   for (i in idxLat) {
# #     tagFinal$lat[i] <- mean(tagFinal$lat[(i-10):(i+10)], na.rm=T)
# #   }
# # }
# if (length(idxLon) > 0) {
#   for (i in idxLon) {
#     tagFinal$lon[i] <- mean(tagFinal$lon[(i-10):(i+10)], na.rm=T)
#   }
# }

# And finally change all the Lons to 360 degree
tagFinal$lon <- ifelse(tagFinal$lon < 0, tagFinal$lon+360, tagFinal$lon)

#-------------------------------------------------------------------------------
# Plot the track so you can see wrong points
library(ggplot2)
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
  mutate(daysAtLiberty=row_number()) %>% 
  ggplot(aes(daysAtLiberty, lat)) +
    geom_path(linewidth=0.5) +
    geom_point(size=2)

tagFinal %>% 
  mutate(daysAtLiberty=row_number()) %>% 
  ggplot(aes(daysAtLiberty, lon)) +
  geom_path(linewidth=0.5) +
  geom_point(size=2)

tagFinal %>% 
  mutate(daysAtLiberty=row_number()) %>% 
  ggplot(aes(daysAtLiberty, sst)) +
  geom_path(linewidth=0.5) +
  geom_point(size=2)



# Remove all rows with more than 1 NA
#tagFinal <- tagFinal[-which(rowSums(is.na(tagFinal))>1),]  

# Save the final tag file for use in UKFSST
# The file need: day month, year, lon, lat, sst
write.csv(tagFinal, paste('FinishedTags_SPC_WC/BET', tag, tagMeta$TagPTT, 'LocSST_forUKFSST_cleaned_outSSTway.csv', sep='_'), row.names = F, quote = F)


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
fit.2 <- kfsst(tagFinal,
                      fix.last = T,
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
### Is the fit good?
plot.kfsst(fit.2, ci=T)


### Did the model converge? Is the fit good?
### Are there any spurious data points that need to be removed?


### Here, I turn by.a back ON and assign starting parameters to see if model 
### performs better
fit.3 <- kfsst(tagFinal,
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
                      u.i = 3.665244, 
                      v.i = 7.294314 , 
                      D.i = 2233.109 ,
                      bx.i = 3.348589 , 
                      by.i = -0.92729    ,
                      bsst.i = 0,
                      sx.i = 2.068778 , 
                      sy.i = 3.624964 , 
                      ssst.i = 0.3294515    , 
                      a0.i = 1.749781e-07, 
                      b0.i = -16.0084)

### See if model converged
print.kfsst(fit.3)
### Is the fit good?
plot.kfsst(fit.3, ci=T)



############################################################################
########################## Export csv file #################################
############################################################################
### Now, let's format our model into the csv source files
setwd(mainDir)
fit2csv(fit = fit.1, name = paste('FinishedTags_SPC_WC/BET', tag, tagMeta$TagPTT, 'UKFSST_BestFitModel_low_fit1_DawnOnly_', sep='_'))
saveRDS(fit.1, paste('FinishedTags_SPC_WC/BET', tag, tagMeta$TagPTT, 'UKFSST_BestFitModel_low_fit1_DawnOnly.rds', sep='_'))

# Plot the ukdsst track and the LAT Viewer track on same figure
cbind(fit.1$date, fit.1$most.prob.track, DAL=fit.1$days.at.liberty) %>% 
ggplot() + borders('world2', fill='gray') +
  geom_point(data=fit.1$nominal.track, aes(x,y), shape='star') +
  geom_path(aes(x,y,color=month), linewidth=1) + 
  geom_point(aes(x,y,color=month), size=2) +
  geom_point(data=tagFinal[c(1,nrow(tagFinal)),], aes(lon, lat), size=5, shape='square') +
  coord_quickmap(xlim = c(min(tagFinal$lon, na.rm=T)-5, max(tagFinal$lon, na.rm=T)+5), ylim = c(min(tagFinal$lat, na.rm=T)-5, max(tagFinal$lat, na.rm=T)+5)) + 
  scale_color_viridis_c(option='turbo')






