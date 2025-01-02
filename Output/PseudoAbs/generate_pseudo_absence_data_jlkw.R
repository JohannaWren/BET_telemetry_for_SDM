
## 1. load libraries
library(sf)
library(raster)
#require("rgdal")
#require("rgeos")
require("dplyr")
library(ggExtra)
#library(maptools)
library(adehabitatHR)
library(here)
library(maps)
library(glue)
library(tidyverse)
library(data.table)
library(lubridate)

##this first bit is a dummy test to see if the function is running ok


## 2. source pseudo-absence functions
source(here("Output/PseudoAbs/create_bkgd_pseudo_absences_function.R"))


## 3. load world data to get coordinate system
# data=maps::map("world2",fill=T)
# IDs <- sapply(strsplit(data$names, ":"), function(x) x[1])
# wrld_simpl <- map2SpatialPolygons(data, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
# wrld=SpatialPolygons(wrld_simpl@polygons,proj4string=wrld_simpl@proj4string) %>%
#   gBuffer(., byid=TRUE, width=0)
## Johanna version w/o maptools
data=maps::map("world2",fill=T)
IDs <- sapply(strsplit(data$names, ":"), function(x) x[1])
wrld_simpl <- st_as_sf(data)
wrld_simpl <- st_transform(wrld_simpl, "+proj=longlat +datum=WGS84")
#wrld <- as(wrld_simpl, Class = "Spatial")
wrld <- as_Spatial(wrld_simpl, cast = TRUE, IDs = paste0("ID", seq_along(wrld_simpl)))


## 4. load global template
# this is an empty global raster at .25 degrees that we will use to ensure pseduo absences are not generated for the same daypixel as presences
# template=raster(here("C:/Users/Molly/OneDrive/Documents/Mel_Molly Projects/SDM Project/R/utilities","template.grd"))
# string=seq(1:ncell(template))
# template[]=string
## Johanna version without a template on hand
# RasterLayer with the default parameters
template <- raster()
# change resolution
res(template) <- 0.25
template
string=seq(1:ncell(template))
template[]=string


## 5. generate some dummy data, comment this out before using, just here to demonstrate how functions work
# how the function expects the dataframe to be formatted ##
# column 'date' -> YYYY-MM-DD in *character* format (will likely need to be reformatted from original data)
# column 'lon' -> 0 to 360 longitude column in numeric format (will likely need to be transformed from -180 to 180 formated in original data)
# column 'lat' -> -90 to 90 latitude column in numeric format (will likely be in correct format in original data)

date=sample(seq(as.Date("2017-01-10"),as.Date("2018-02-10"),by="day"),size=300,replace = T) %>% as.character()
lon=sample(seq(159,260,by=.25),size=length(date),replace = T)
lat=sample(seq(10,50,by=.25),size=length(date),replace = T)
othervar1=rep("test",length(date)) # added to make sure function carries thru other attributes
othervar2=rep("test2",length(date))# added to make sure function carries thru other attributes
df=data.frame(lon=as.numeric(lon),
              lat=as.numeric(lat),
              date=as.character(date),
              size=as.character(othervar1),
              tagID=as.character(othervar2),
              stringsAsFactors = F)

## 6. run the function
csvdir=here("Output/PseudoAbs")
polydir=here("Output/PseudoAbs")
species="test"
generate_pseudo_abs_fcn(dat=df,
                        csv_outdir=csvdir,
                        poly_outdir=polydir,
                        sp_name=species)


## 7. check what happened
PA_dat=read.csv(glue("{csvdir}/{species}_presAbs.csv"))
poly=st_read(glue("{polydir}/{species}.shp"))

minx = min(PA_dat$lon)
maxx = max(PA_dat$lon)
miny = min(PA_dat$lat)
maxy = max(PA_dat$lat)


#looks like some presence go over land?!
ggplot()+
  geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
  geom_point(data=PA_dat,aes(x=lon,y=lat,color=as.factor(presAbs),group=presAbs),size = 1, shape = 21)+
  geom_sf(data=poly,fill=NA,color="black")+
  scale_color_manual("PresAbs",values=c("1"="blue","0"="red"))+
  coord_sf(xlim = c(minx, maxx), ylim = c(miny,maxy))+
  ggtitle(glue("{species} npre={nrow(PA_dat)} (1:1 ratio)"))


################################################################################################################################################################
### ############################################################################################################################################################

#this section is with our own data.. 

## READ  IN ALL UKFSST.CSVS - make sure they are in the same folder.

# ##regex - Final data/Perfect_data
# files_to_read = list.files(
#   path = "C:/Users/Molly/OneDrive/Documents/Mel_Molly Projects/SDM Project/BSH_UKFSST",   # directory to search within
#   pattern = ".*(UKFSST).*csv$", # regex pattern, some explanation below
#   recursive = TRUE,          # search subdirectories
#   full.names = TRUE          # return the full path
# )
# 
# 
# data_lst = lapply(files_to_read, read.csv)  # read all the matching files
# 
# #MAKE LIST INTO ONE BIG DATAFRAME
# 
# bsh.tracks = rbindlist(data_lst) #very good function to combine dataframes from lists 
bsh.tracks <- read.csv(here('Output/Tag_BET_All_MPTs.csv'))

#for the pseudo-absence dataframe we need to have a date column, so we need to concatenate day, month, year

bsh.tracks = bsh.tracks %>%
  mutate(date = make_datetime(year, month, day))

#change tag.serial to tagID

names(bsh.tracks)[names(bsh.tracks)=="tag.serial"]<-"tagID"

#for the  SDMs the SWFSC want only one position per day so we are going to choose our one position based on the 'observation type'
#we want to create a nested if-else statement to return lat longs based on observation types in this order (of most to least accurate)

bsh.tracks = unique(bsh.tracks) #one location per day


# #CODE TO PARSE ALL DATES TO SAME FORMAT!
# bsh.tracks$Date.Time = parse_date_time(bsh.tracks$Date.Time, orders = c('%d-%b-%Y %H:%M:%S', '%d/%m/%Y %H:%M', "%m/%d/%Y %H:%M"))
# 
# bsh.tracks$Date.Time = as.character(bsh.tracks$Date.Time)


#add in SPAT locatiosn ?? - DO WE CARE ABOUT SPAT LOCATIONS?

#spat = read_csv("C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/SWFSC/BSH_SPAT_locs.csv")


#re-create bsh.tracks from out.df  - this has to be named exactly these values
#change name of dt col to date because it has to be in exact same format as this: 

#need to be this format AND EXACT SAME NAMES
# df=data.frame(lon=as.numeric(lon),
#               lat=as.numeric(lat),
#               date=as.character(date),
#               size=as.character(othervar1),
#               tagID=as.character(othervar2),
#               stringsAsFactors = F)


#need to change name of dt column

bsh.tracks = bsh.tracks %>%
  dplyr::select(lon,lat,date,tagID)

#date needs to 

#sp$DeployID = as.factor(sp$DeployID)

#convert lat and lon - very important step!

bsh.tracks$lon[bsh.tracks$lon < 0] <- bsh.tracks$lon[bsh.tracks$lon < 0] + 360 

bsh.tracks$tagID = as.factor(bsh.tracks$tagID)

# with(bsh.tracks, table(Observation.Type))

#create pseudo-absences ** 

## 6. run the function
csvdir=here("Output/PseudoAbs")
polydir=here("Output/PseudoAbs")
species="BET"
generate_pseudo_abs_fcn(dat=bsh.tracks,
                        csv_outdir=csvdir,
                        poly_outdir=polydir,
                        sp_name=species)


## 7. check what happened
PA_bsh_all=read.csv(glue("{csvdir}/{species}_presAbs.csv"))
poly=st_read(glue("{polydir}/{species}.shp"))

minx = min(PA_bsh_all$lon)
maxx = max(PA_bsh_all$lon)
miny = min(PA_bsh_all$lat)
maxy = max(PA_bsh_all$lat)


#plot poly w presence/ abs

ggplot()+
  geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
  geom_point(data=PA_bsh_all,aes(x=lon,y=lat,color=as.factor(presAbs),group=presAbs),size = 0.5, shape = 21)+
  geom_sf(data=poly,fill=NA,color="black")+
  scale_color_manual("PresAbs",values=c("1"="blue","0"="red"))+
  coord_sf(xlim = c(176, 260), ylim = c(miny,maxy), expand=FALSE)#+
  #ggtitle(glue("{species} npre={nrow(PA_sp_all)} (1:1 ratio)"))


############################# STOP HERE #################################

#this code is for if we were running pseudo-absence with the GPE3.csvs and needed one location per day.



# with(bsh.tracks, table(Observation.Type))
# 
# #convert to date to be able to bind rows 
# spat$date <- as.Date(spat$date, format= "%d-%b-%y")
# 
# bsh.try = bind_rows(bsh.tracks, spat)
# 
# bsh.tracks = bsh.try
# 
# #turn date back into characeter for loop
# 
# bsh.tracks$date = as.character(bsh.tracks$date)


# #User - Argos3-  Argos2 - Argos1- Argos0- Light Dawn - Light Dusk - SST - None i.e. select the lat/long for a day in this order
# 
# #check what types of observation type exist in your dataframe. Whatever the categories are, change the order of preferece in order_pref L226 below
# with(bsh.tracks, table(Observation.Type))
# 
# 
# #make df of unique tag_dates
# bsh.tracks$tag_dt <- paste(bsh.tracks$tagID, bsh.tracks$date, sep="_")
# 
# out_df <- tibble(tg_dt = unique(bsh.tracks$tag_dt))
# 
# # probably an easier way to do this, but this works
# out_df$tagID <- sapply(out_df$tg_dt, 
#                        function(x){strsplit(x, "_")[[1]][1]})
# 
# out_df$dt    <- as.Date(sapply(out_df$tg_dt, 
#                                function(x){strsplit(x, "_")[[1]][2]}))
# 
# # make dates typ Date and factor as character
# bsh.tracks$date             <- as.Date(bsh.tracks$date)
# bsh.tracks$Observation.Type <- as.character(bsh.tracks$Observation.Type)
# 
# # order preference
# order_pref <- c( "User","Argos-3", "Argos-2", "Argos-1", "Argos-0", "Light - Dawn", "Light - Dusk",
#                  "SST", "None")
# 
# # set up placeholders
# out_df$lat              <- NA
# out_df$lon              <- NA
# out_df$Observation.Type <- NA
# 
# # set up if else for values
# for(i in 1:nrow(out_df)){
#   
#   #get dat and tag for row
#   tag_i = out_df$tagID[i]
#   dt_i  = out_df$dt[i]
#   
#   # get data that matters
#   tag_dt_i <- bsh.tracks[bsh.tracks$tagID == tag_i & 
#                            bsh.tracks$date == dt_i, ]
#   
#   # fill in value
#   for(pref_j in order_pref){
#     
#     # check if still NA
#     if(is.na(out_df$lat[i])){
#       
#       # if so, check if we have any pref_j, then fill in
#       if(sum(tag_dt_i$Observation.Type == pref_j) > 0){
#         
#         # just snag first value if there are multiple
#         out_df$lat[i] <- tag_dt_i$lat[tag_dt_i$Observation.Type == pref_j][1]
#         out_df$lon[i] <- tag_dt_i$lon[tag_dt_i$Observation.Type == pref_j][1]
#         
#         # add in obs type
#         out_df$Observation.Type[i] <- 
#           tag_dt_i$Observation.Type[tag_dt_i$Observation.Type == pref_j][1]
#         
#         
#       }
#     }
#   }
# }
# 
# head(out_df)

#change name of dt to 'date' to align with exact format required

names(out_df)[names(out_df)=="dt"]<-"date"

#change date to character for function

out_df$date = as.character(out_df$date)



















