#-------------------------------------------------------------------------------
# Date: June 26, 2024
# Author: Johanna Wren
# Email: johanna.wren@noaa.gov
# Description: Processing data output from LatViewer for use in UKFSST location processing. 
#              This script calculates average daily nighttime temperature and add that to the
#              location file from LatViewer, removes missing location data, and puts it in a 
#              format for use in  ukfsst. This code and method is based off Mitch Lovell's 
#              "Location Processing Protocol_for Lotek.docx" protocol and translated into R. 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Clear workspace
rm(list=ls())

# Set working directory
mainDir <- "C:/Users/Johanna.Wren/Desktop/New Archival Tag Data 2024/Lotek/"

setwd(mainDir)

#-------------------------------------------------------------------------------
# Load Libraries
library(dplyr)
library(chron)
library(lubridate)
library(ggplot2)

#-------------------------------------------------------------------------------
# Load Meta data
#-------------------------------------------------------------------------------
# Tag metadata
meta <- read.csv('../../Updated Archival Tag Data 2024.csv', na.strings=c(""," ","NA"))
# Changing the metadata file to match the code column names (added this for the new SPC files, comment out if running 'old' SPC files)
meta <- meta %>%
  rename(Tagging.DeployLatitude=release_lat, Tagging.DeployLongitude_180=release_lon,
         Capture.FateDate=best_catchdate, Tagging.DeployDateLocal=release_date, TagPTT=Arc_tag_no,
         Recovery.PopoffLong=recov_lon, Recovery.PopoffLat=recov_lat)
# Make time zone record based on tagging lon/lat becuase I don't understand SPC's time zones
meta$tagTZ <-lutz::tz_lookup_coords(lat = meta$Tagging.DeployLatitude, lon = meta$Tagging.DeployLongitude_180, method = 'accurate')

#-------------------------------------------------------------------------------
# SET TAG NUMBER (Ptt)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# This is generally the Ptt number, but it's up to you. Just make sure that it matches what you are searching
# for in the next section when you read in the data. 
tag <- 'L01687'
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Load Data
#-------------------------------------------------------------------------------
# Read in LatViewer Files
tagFiles <- list.files(path='ExtractedTags_SPC_Lotek_new/', pattern=paste0(tag), full.names = T)
tagFiles
# CSV file from LatViewer
tagDat <- read.csv(tagFiles[grep(paste0(tag, '.csv'), tagFiles)], na.strings=c(""," ","NA"))
# Temperature file from LatViewer
tagTemp <- read.csv(tagFiles[grep('ExtTemp', tagFiles)])[,1:2]
# Depth file from LatViewer
tagPres <- read.csv(tagFiles[grep('Pressure', tagFiles)])[,1:2]

#-------------------------------------------------------------------------------
# Process data
#-------------------------------------------------------------------------------
#  Pull out the metadata record for your tag
tagMeta <- meta %>% 
  filter(TagPTT == tag) %>% 
  mutate(DeployDT=as.POSIXct(Tagging.DeployDateLocal, format='%d/%m/%Y', tz=tagTZ), 
         DeployDate=date(DeployDT), 
         RecoverDT=as.POSIXct(Capture.FateDate, format='%d/%m/%Y', tz=tagTZ), 
         RecoverDate=date(RecoverDT))
tagMeta

# Change some data formats and limit the range of the data to tagging start and end dates
# Change date to date format
tagDat$dd.mm.yy <- as.Date(tagDat$dd.mm.yy, format='%d/%m/%y')
tagTemp$TimeS <- as.POSIXct(tagTemp$TimeS, format='%H:%M:%S %d/%m/%y', tz='UTC')
tagTemp$DT_local <- with_tz(tagTemp$TimeS, tagMeta$tagTZ)
tagPres$TimeS <- as.POSIXct(tagPres$TimeS, format='%H:%M:%S %d/%m/%y', tz='UTC')
tagPres$DT_local <- with_tz(tagPres$TimeS, tagMeta$tagTZ)

# Change sunrise and sunset to time formats in UTC
tagDat$SunriseDT <- tagDat$dd.mm.yy + hm(tagDat$Sunrise)
tagDat$SunsetDT <- tagDat$dd.mm.yy + hm(tagDat$Sunset)
tagDat$NoonDT <- tagDat$dd.mm.yy + hm(tagDat$Noon)
tagDat$MidnightDT <- tagDat$dd.mm.yy + hm(tagDat$Midnight)
# Then change that to local time for the tag
tagDat$SunriseDTlocal <- with_tz(tagDat$SunriseDT, tz=tagMeta$tagTZ)
tagDat$SunsetDTlocal <- with_tz(tagDat$SunsetDT, tz=tagMeta$tagTZ)
tagDat$NoonDTlocal <- with_tz(tagDat$NoonDT, tz=tagMeta$tagTZ)

# Subtract one hour from Sunrise (this value will be used to filter raw data when obtaining sst values)
tagDat$Sunrise_less1 <- format(tagDat$SunriseDTlocal-3600, '%H:%M')
# Add one hour to Sunset (this value will be used to filter raw data when obtaining sst values)
tagDat$Sunset_plus1 <- format(tagDat$SunsetDTlocal+3600, '%H:%M')

# Remove all dates before and after the tag date
tagDat <- tagDat %>% 
  filter(dd.mm.yy >= tagMeta$DeployDate & dd.mm.yy <= tagMeta$RecoverDate)

# Merge temperature and pressure data from tag and remove points outside the tag dates. 
# Make a time column and a date column
tagEnv <- tagTemp %>% 
  dplyr::select(-TimeS) %>% 
  left_join(tagPres, by='DT_local') %>% 
  filter(DT_local >= paste(tagMeta$DeployDate,'00:00:00') & DT_local <= paste(tagMeta$RecoverDate, '23:59:59')) %>% 
  mutate(Date = as.Date(format(DT_local, '%Y-%m-%d')), 
         Time = format(DT_local, '%H:%M'), 
         TimeHMS=times(format(DT_local, '%H:%M:%S')))

# Quickly plot the environmental data to make sure the input looks right
ggplot(tagEnv, aes(DT_local, Pressure, color=ExtTemp)) + 
  geom_path() + 
  scale_y_reverse() + 
  scale_color_viridis_c(option = 'turbo')

#-------------------------------------------------------------------------------
# Pull out night time first, THEN shallow depths
tagNight <- list()
for (i in seq_along(tagDat$dd.mm.yy)) {
  tagNight[[i]] <- tagEnv %>%
    filter(Date == tagDat$dd.mm.yy[i]) %>%
    filter(Time < tagDat$Sunrise_less1[i] | Time > tagDat$Sunset_plus1[i])
}
tagNightAllDepth <- data.table::rbindlist(tagNight) %>%
  mutate(DT_night = DT_local+(60*60*12), Date_night = as.Date(ymd_hms(DT_night))) %>%
  data.frame()

for (d in seq(10,100, by=5)) {
  # Pull out all temps shallower than d meters
  tagEnvShallow <- tagNightAllDepth %>%
    filter(Pressure <= d)
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

tagNightDF <- tagEnvShallow %>%
  group_by(Date_night) %>%
  summarize(sst=mean(ExtTemp)) %>%
  mutate(Date = Date_night-(1)) %>%
  data.frame()

#-------------------------------------------------------------------------------
# Join the average night time time from above with TRLon and TRlat from the tagDat file
# Separate dates into day, month, year
tagFinal <- tagDat %>% 
  rename(Date=dd.mm.yy, lon=TRLon, lat=TRLat) %>% 
  dplyr::select(Date, lon, lat) %>% 
  full_join(tagNightDF) %>% 
  mutate(day=day(Date), month=month(Date), year=year(Date)) %>% 
  dplyr::select(day, month, year, lon, lat, sst)

# Deploy and recover info
data.frame(Date=c(tagMeta$DeployDate, tagMeta$RecoverDate), Lon=c(tagMeta$Tagging.DeployLongitude_180, tagMeta$Recovery.PopoffLong), Lat=c(tagMeta$Tagging.DeployLatitude, tagMeta$Recovery.PopoffLat))
head(tagFinal)
# Replace the release and capture locations with the known release and recapture locations
tagFinal[1,c('lon', 'lat')] <- tagMeta[,c('Tagging.DeployLongitude_180', 'Tagging.DeployLatitude')]
# Make sure you don't have artifacts from joining at the end of your dataframe. If you do, remove them
tail(tagFinal)
# Remove last record if needed
tagFinal <- tagFinal[-nrow(tagFinal),]
# Add last row of metadata recovery locations if needed
tagFinal[nrow(tagFinal),] <- data.frame(day=day(tagMeta$RecoverDate), month=month(tagMeta$RecoverDate), year=year(tagMeta$RecoverDate), lon=as.numeric(tagMeta$Recovery.PopoffLong), lat=as.numeric(tagMeta$Recovery.PopoffLat), sst=NA)
tagFinal[nrow(tagFinal),c('lon', 'lat')] <- as.numeric(tagMeta[,c('Recovery.PopoffLong', 'Recovery.PopoffLat')])
# This is to remove half days of data that may be wrong
tagFinal$sst[nrow(tagFinal)] <- NA 
tail(tagFinal)

#-------------------------------------------------------------------------------
# Remove missing values intruduced by LatViewer Studio
# LatViewer sometimes will add a lon=200 or lat=100 and SST=-20 as missing values so we need to go through and give those slots an average value instead
tagFinal$lat[which(tagFinal$lat == 100)] <- NA
tagFinal$lon[which(tagFinal$lon == 200)] <- NA
tagFinal$sst[which(tagFinal$sst == -20)] <- NA

# And finally change all the Lons to 360 degree
tagFinal$lon <- ifelse(tagFinal$lon < 0, tagFinal$lon+360, tagFinal$lon)

#-------------------------------------------------------------------------------
# Plot the track so you can see wrong points
ggplot(tagFinal, aes(lon, lat, color=sst)) + 
  borders('world2', fill='gray') +
  geom_path() + 
  geom_point() +
  geom_point(data=tagFinal[c(1,nrow(tagFinal)),], aes(lon, lat), size=3, color='black', shape=c(19,17)) +
  coord_quickmap(xlim = c(min(tagFinal$lon, na.rm = T)-5, max(tagFinal$lon, na.rm=T)+5), ylim = c(min(tagFinal$lat, na.rm=T)-5, max(tagFinal$lat, na.rm=T)+5)) + 
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



# Tag L01687
tagFinal <- tagFinal[1:228,]
tagFinal$lat[which(tagFinal$lat > 7.5 | tagFinal$lat < -6)] <- NA
tagFinal$lon[which(tagFinal$lon > 210 | tagFinal$lon < 182)] <- NA
tagFinal$lon[c(82,84,85,143,159,174,179,184:189,207,210,213,218)] <- NA

# # Tag L02205
# tagFinal <- tagFinal[1:103,]
# tagFinal$lat[which(tagFinal$lat > 10 | tagFinal$lat < -10)] <- NA
# tagFinal$lon[which(tagFinal$lon > 198 | tagFinal$lon < 180)] <- NA
# 
# # Tag L02212
# tagFinal <- tagFinal[1:160,]
# tagFinal$lat[which(tagFinal$lat > 8 | tagFinal$lat < -7.5)] <- NA
# tagFinal$lon[which(tagFinal$lon > 205 | tagFinal$lon <= 188)] <- NA
# tagFinal$lon[c(32,54,104,113,140)] <- NA
# tagFinal$sst[c(17,18,43:45,79,80)] <- NA

# # Tag L02202
# tagFinal$lat[which(tagFinal$lat > 10 | tagFinal$lat < -5)] <- NA
# tagFinal$lat[c(15,16,39,47:49,183,205,206,186)] <- NA
# tagFinal$lon[which(tagFinal$lon > 220 | tagFinal$lon < 195)] <- NA
# tagFinal$lon[c(53:71,94,151,198,204,211)] <- NA

# # Tag L01683
# tagFinal <- tagFinal[1:180,]
# tagFinal$lat[which(tagFinal$lat > 7 | tagFinal$lat < -5)] <- NA
# tagFinal$lon[which(tagFinal$lon > 210 | tagFinal$lon < 193)] <- NA
# tagFinal$lon[c(41,114,138,172)] <- NA
# tagFinal$sst[which(tagFinal$sst < 20)] <- NA

# # Tag L02295
# tagFinal <- tagFinal[1:64,]
# tagFinal$lat[which(tagFinal$lat > 25 | tagFinal$lat < -10)] <- NA
# tagFinal$lon[which(tagFinal$lon > 200 | tagFinal$lon < 180)] <- NA
# tagFinal[57,'lon'] <- NA

# # Tag L02243
# tagFinal <- tagFinal[1:86,]
# tagFinal$lat[which(tagFinal$lat > 10 | tagFinal$lat < -10)] <- NA
# tagFinal$lon[which(tagFinal$lon > 200 | tagFinal$lon < 180)] <- NA
# tagFinal$lon[c(21,33,41,66)] <- NA

# # Tag L01696
# tagFinal$sst[which(tagFinal$sst < 20)] <- NA
# tagFinal$lat[which(tagFinal$lat < -10 | tagFinal$lat > 20)] <- NA
# tagFinal$lat[c()] <- NA
# tagFinal$lon[which(tagFinal$lon < 191 | tagFinal$lon > 210)] <- NA
# tagFinal$lon[c(106,118,133,165,177,184,194)] <- NA
# tagFinal <- tagFinal[1:90,]

# # Tag L02216
# tagFinal$lat[which(tagFinal$lat < -5 | tagFinal$lat > 8)] <- NA
# tagFinal$lat[c(36,134,179)] <- NA
# tagFinal$lon[which(tagFinal$lon < 185 | tagFinal$lon > 222)] <- NA
# tagFinal$lon[c(36,134,146,172,179)] <- NA
# tagFinal$sst[which(tagFinal$sst < 20)] <- NA

# # Tag L02238
# tagFinal <- tagFinal[1:136,]
# tagFinal$lat[which(tagFinal$lat <= -10 | tagFinal$lat > 20)] <- NA
# tagFinal$lat[c(72,110, 68:86)] <- NA
# tagFinal$lon[which(tagFinal$lon > 205 | tagFinal$lon < 180)] <- NA
# tagFinal$lon[c(110,117,126,129,128,135)] <- NA

# # Tag L02213
# tagFinal <- tagFinal[1:109,]
# tagFinal$sst[which(tagFinal$sst < 24)] <- NA
# tagFinal$lat[which(tagFinal$lat < -15)] <- NA
# tagFinal$lat[c(80,94,59)] <- NA
# tagFinal$lon[which(tagFinal$lon > 200 | tagFinal$lon < 170)] <- NA
# tagFinal$lon[c(53,69,75,94)] <- NA

# # Tag L02236
# tagFinal$lat[which(tagFinal$lat < -5 | tagFinal$lat > 10)] <- NA
# tagFinal$lon[which(tagFinal$lon < 183)] <- NA
# tagFinal$sst[which(tagFinal$sst < 24)] <- NA
# tagFinal$lat[c(61:72,86,91,129)] <- NA

# # Tag L02263
# tagFinal$lat[which(tagFinal$lat < -9 | tagFinal$lat > 15)] <- NA
# tagFinal$lat[c(24,33,98)] <- NA
# tagFinal$lon[which(tagFinal$lon < 178 | tagFinal$lon > 210)] <- NA
# tagFinal$lon[c(12,14,16,60,108,114)] <- NA
# tagFinal$sst[which(tagFinal$sst < 25)] <- NA

# # Tag L02274
# tagFinal$lat[which(tagFinal$lat > 20 | tagFinal$lat < -5)] <- NA
# tagFinal$lon[which(tagFinal$lon < 195)] <- NA
# tagFinal$sst[which(tagFinal$sst <27)] <- NA

# # Tag  L02220
# tagFinal$lat[which(tagFinal$lat > 6 | tagFinal$lat < -5)] <- NA

# # Tag L01682
# tagFinal$lat[15:16] <- NA

# # Tag L02253
# tagFinal$lat[which(tagFinal$lat < 0)] <- NA

# # Tag L02234
# tagFinal$lat[which(tagFinal$lat < -8 | tagFinal$lat > 10)] <- NA
# tagFinal$lat[c(98,128,156,138,183,184,188,203,213,215)] <- NA
# tagFinal$lon[which(tagFinal$lon < 170 | tagFinal$lon > 220)] <- NA
# tagFinal$lon[c(76,90,122,173:177,187,212)] <- NA
# tagFinal$sst[nrow(tagFinal)-1] <- NA

# # Tag L02294
# tagFinal$lat[which(tagFinal$lat > 25 | tagFinal$lat < -10)] <- NA
# tagFinal$lon[which(tagFinal$lon > 220 | tagFinal$lon < 160)] <- NA
# tagFinal$sst[which(tagFinal$sst < 22)] <- NA
# tagFinal$lon[c(42,86)] <- NA

# # Tag 913
# tagFinal <- tagFinal %>% filter(sst > 20)
# tagFinal$sst[which(tagFinal$sst > 31)] <- NA
# tagFinal$lon[which(tagFinal$lon <= 155)] <- NA
# tagFinal$lon[which(tagFinal$lon >= 175)] <- NA
# tagFinal$lon[c(9,188,219)] <- NA
# tagFinal$lat[which(tagFinal$lat <= -50)] <- NA
# tagFinal$lat[which(tagFinal$lat <= -20)][1:11] <- NA
# tagFinal$lat[which(tagFinal$lat >= 25)][11:38] <- NA

# Tag 12749
# Long and lat data so bad I don't know how to clean

# # Tag A0738
# tagFinal$sst[60] <- NA
# tagFinal$lat[which(tagFinal$lat > 30)] <- NA
# tagFinal$lat[which(tagFinal$lat < 0)] <- NA

# # Tag A0692
# tagFinal$lat[which(tagFinal$lat > 20 | tagFinal$lat < 0)] <- NA
# tagFinal$lon[which(tagFinal$lon > 250 | tagFinal$lon < 200)] <- NA
# 
# # Tag A0720
# tagFinal$lat[which(tagFinal$lat < -24)] <- NA
# tagFinal$lat[which(tagFinal$lat > 23)] <- NA
# tagFinal$lat[138] <- NA
# tagFinal$lon[which(tagFinal$lon < 200)] <- NA
# tagFinal$lon[which(tagFinal$lon < 225)][115:212] <- NA
# tagFinal$lon[c(39,49,62,86,166,170,178,179,181,190,203,208,210,212,215,222,224,229)] <- NA

# # A0576
# tagFinal$sst[nrow(tagFinal)-1] <- NA #16.94738
# tagFinal$lon[which(tagFinal$lon > 210)] <- NA
# tagFinal$lat[which(tagFinal$lat < -5)][1] <- NA

# # Tag A0721
# # Don't know how to clean it's so messy.
# tagFinal <- tagFinal[1:114,]
# tagFinal$lat[which(tagFinal$lat < -0.1)] <- NA
# tagFinal$lat[106] <- NA
# tagFinal$lon[which(tagFinal$lon > 240 | tagFinal$lon < 200)] <- NA
# tagFinal$sst[1] <- NA

# 
# # Tag A0717
# tagFinal$lat[which(tagFinal$lat > 25 | tagFinal$lat < -20)] <- NA
# tagFinal$lon[which(tagFinal$lon > 270 | tagFinal$lon < 220)] <- NA
# tagFinal$lon[c(6,12,13,15,34,36,38,47,74,76,80,86,128,130,138,139,141,144,150,154,163,164,166,167,170,171,174,179,182,208,215,221:nrow(tagFinal))] <- NA
# tagFinal$sst[which(tagFinal$sst < 22)] <- NA


# # Tag 0228
# tagFinal$lat[which(tagFinal$lat > 25 | tagFinal$lat < -25)] <- NA

# # Tag 132
# tagFinal$lat[which(tagFinal$lat > 10)] <- NA
# tagFinal[42,'lon'] <- NA

# # Tag D5515
# tagFinal <- tagFinal[-51,]
# tagFinal$lat[which(tagFinal$lat > 20)] <- NA
# tagFinal$lon[which(tagFinal$lon > 223)] <- NA

# # Tag D4877
# tagFinal$sst[nrow(tagFinal)-1] <- NA
# tagFinal$lat[which(tagFinal$lat > 15)] <- NA

# # Tag D5511
# tagFinal$sst[138] <- NA  # This one is way lower than the rest
# tagFinal$lon[which(tagFinal$lon > 230 | tagFinal$lon < 219)] <- NA
# tagFinal$lat[which(tagFinal$lat > 10)][1:4] <- NA # Leaving the later high lon first, then if not converging I'll remove them
# tagFinal$lat[which(tagFinal$lat < - 28)] <- NA

# # Tag "D5480"
# tagFinal$lat[which(tagFinal$lat < -30)] <- NA
# tagFinal$lat[which(tagFinal$lat > 3.5)] <- NA
# tagFinal$lon[which(tagFinal$lon < 210)] <- NA
# tagFinal$lon[which(tagFinal$lon > 232)] <- NA  
# tagFinal$lon[c(24,29,34)] <- NA
# tagFinal$sst[which(tagFinal$sst < 23)] <- NA

# # Tag A0720 cleaning
# tagFinal[c(138), 'lat'] <- NA
# tagFinal[which(tagFinal$lat <= -20),'lat'] <- NA
# tagFinal[which(tagFinal$lat >= 20),'lat'] <- NA
# tagFinal <- tagFinal[-c(39, 48,49,58,62, 83,86,88,89,107,144,160,161,166,169,170,175,179,181,184,190,200,203,208:212,215,222,224,229),]
# tagFinal <- tagFinal[-c(nrow(tagFinal)-1, nrow(tagFinal)),]

# # Tag "D5136"
# tagFinal$lat[which(tagFinal$lat <= -9)] <- NA

# # Tag D5485
# tagFinal$sst[which(tagFinal$sst < 24)] <- NA
# tagFinal$lon[which(tagFinal$lon < 216)] <-NA
# tagFinal$lon[which(tagFinal$lon > 230)] <- NA
# tagFinal$lat[which(tagFinal$lat > 12)] <- NA

# # Tag D5488
# tagFinal$sst[which(tagFinal$sst < 20)] <- NA
# tagFinal$lat[which(tagFinal$lat  > 12.5 | tagFinal$lat < -10)] <- NA
# tagFinal$lat[c(8,91)] <- NA
# tagFinal$lon[c(8,127)] <- NA

# # Tag 0228
# tagFinal$lat[which(tagFinal$lat > 15 | tagFinal$lat < -18)] <- NA


# Remove all rows with more than 1 NA
tagFinal <- tagFinal[-which(rowSums(is.na(tagFinal))>1),]  

# Save the final tag file for use in UKFSST
# The file need: day month, year, lon, lat, sst
write.csv(tagFinal, paste('FinishedTags_SPC_Lotek_new/BET', tagMeta$tag_no, tag, 'DateLonLatSST', paste0(d,'m'), 'cleaned.csv', sep='_'), row.names = F, quote = F)


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
source("../../fit2csv.R")
source("../../get.sst.from.server_downloaded.R") # HIGH Resolution (can be adjusted in the get.sst.from.server source file)


# Check the final tag to make sure you have the start dates you should
head(tagFinal)
# And sometimes when you join above you get the records that can't join tacked on at the end. These are usually the first day of the tag
# so make sure you don't have a faulty record here because it messes up the get.sst.from.server function
tail(tagFinal)

### The sst.path is stored in the environment and used by kfsst() model below
### You only have to run this once if you save your Work Environment
sst.path <- suppressWarnings(get.sst.from.server.jlkw(tagFinal, res='high'))


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
### Is the fit good?
plot.kfsst(fit.2, ci=T)


# # which model has best negative log likelihood?
# fit.1$nlogL
# fit.2$nlogL
# 
# 
# ### Did the model converge? Is the fit good?
# ### Are there any spurious data points that need to be removed?
# 
# 
# ### Here, I turn by.a back ON and assign starting parameters to see if model 
# ### performs better
# fit.3 <- kfsst(tagFinal,
#                       fix.last = F,
#                       u.a = T, 
#                       v.a = T, 
#                       D.a = T, 
#                       bx.a = F, 
#                       by.a = T, 
#                       bsst.a = F,
#                       sx.a = T,
#                       sy.a = T,
#                       ssst.a = T,
#                       u.i =  -3.63437178541, 
#                       v.i = 1.97052339975, 
#                       D.i = 947.774227093,
#                       bx.i = 0, 
#                       by.i = 2.69190016225,
#                       bsst.i = 0,
#                       sx.i = 0.436268029494, 
#                       sy.i = 0.320123347819,
#                       ssst.i = 0.324327358938, 
#                       a0.i = 0.0428611833647, 
#                       b0.i = 52.126388173)
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
finMod <- fit.2
finVar <- 'fit2'
fit2csv(fit = finMod, name = paste('FinishedTags_SPC_Lotek_new/BET', tagMeta$tag_no, tag, 'UKFSST_BestFitModel', SSTres, finVar, '', sep='_'))
saveRDS(finMod, paste('FinishedTags_SPC_Lotek_new/BET', tagMeta$tag_no, tag, 'UKFSST_BestFitModel', SSTres, finVar, '.rds', sep='_'))

# Plot the ukdsst track and the LAT Viewer track on same figure
cbind(finMod$date, finMod$most.prob.track, DAL=finMod$days.at.liberty) %>%
ggplot() + borders('world2', fill='gray') +
  geom_point(data=finMod$nominal.track, aes(x,y), shape='o') +
  geom_path(aes(x,y,color=month), linewidth=1) +
  geom_point(aes(x,y,color=month), size=2) +
  #geom_point(data=tagFinal[c(1,nrow(tagFinal)),], aes(lon, lat), size=5, shape='square') +
  coord_quickmap(xlim = c(min(finMod$most.prob.track$x, na.rm=T)-5, max(finMod$most.prob.track$x, na.rm=T)+5), ylim = c(min(finMod$most.prob.track$y, na.rm=T)-5, max(finMod$most.prob.track$y, na.rm=T)+5)) +
  scale_color_viridis_c(option='turbo')

# # Turn model ouput into standard format to pass on to SDM team
# final <- cbind(finMod$most.prob.track, finMod$date, finMod$SST[,'o'])
# #final$tag.serial <- ifelse(is.na(tagMeta$TagSerial), tag, tagMeta$TagSerial)
# final$tag.serial <- ifelse(is.na(tagMeta$tag_no), tag, tagMeta$tag_no)
# colnames(final)[c(1,2,6)] <- c('lon', 'lat', 'sst')
# final <- final[,c('tag.serial', 'year', 'month', 'day', 'lon', 'lat', 'sst')]
# final
# write.csv(final, paste('../../FinalOutputTags/BET', tagMeta$tag_no, tag, 'UKFSSTout', SSTres, finVar, '.csv', sep='_'))


# ############################################################################
# ########################## Export csv file #################################
# ############################################################################
# setwd(mainDir)
# #lotekFiles <- list.files(path='SPC_Lotek/FinishedTags_SPC_Lotek/', pattern='.rds', full.names = T)
# lotekFiles <- list.files(path='Lotek/FinishedTags_SPC_Lotek_new/', pattern='.rds', full.names = T)
# ### Now, let's format our model into the csv source files
# i=1
# lotekFiles[i]
# tagDeets <- strsplit(lotekFiles[i], split='_')
# finMod <- readRDS(lotekFiles[i])
# SSTres <-  tagDeets[[1]][9]
# finVar <- substr( tagDeets[[1]][7], start = 1, stop = 4)
# 
# # # 
# # fit2csv(fit = fit.1, name = paste('FinishedTags_SPC/BET', tag, tagMeta$TagSerial, 'UKFSST_BestFitModel_low_fit1_', sep='_'))
# # saveRDS(fit.1, paste('FinishedTags_SPC/BET', tag, tagMeta$TagSerial, 'UKFSST_BestFitModel_low_fit1.rds', sep='_'))
# 
# #
# print.kfsst(finMod)
# plot.kfsst(finMod, ci=T)
# 
# # Plot the ukdsst track and the LAT Viewer track on same figure
# cbind(finMod$date, finMod$most.prob.track, DAL=finMod$days.at.liberty) %>% 
# ggplot() + borders('world2', fill='gray') +
#   geom_point(data=finMod$nominal.track, aes(x,y), shape='o') +
#   geom_path(aes(x,y,color=month), linewidth=1) + 
#   geom_point(aes(x,y,color=month), size=2) +
#   #geom_point(data=tagFinal[c(1,nrow(tagFinal)),], aes(lon, lat), size=5, shape='square') +
#   coord_quickmap(xlim = c(min(finMod$most.prob.track$x, na.rm=T)-5, max(finMod$most.prob.track$x, na.rm=T)+5), ylim = c(min(finMod$most.prob.track$y, na.rm=T)-5, max(finMod$most.prob.track$y, na.rm=T)+5)) + 
#   scale_color_viridis_c(option='turbo')
# 
# # Turn model ouput into standard format to pass on to SDM team
# final <- cbind(finMod$most.prob.track, finMod$date, finMod$SST[,'o'])
# #final$tag.serial <- ifelse(is.na(tagMeta$TagSerial), tag, tagMeta$TagSerial)
# #final$tag.serial <- ifelse(is.na(tagMeta$tag_no), tag, tagMeta$tag_no)
# final$tag.serial <- tagDeets[[1]][6]
# colnames(final)[c(1,2,6)] <- c('lon', 'lat', 'sst')
# final <- final[,c('tag.serial', 'year', 'month', 'day', 'lon', 'lat', 'sst')]
# final
# write.csv(final, paste('FinalOutputTags/BET', tagDeets[[1]][6], tagDeets[[1]][5], 'UKFSSTout', SSTres, finVar, '.csv', sep='_'))
# 





