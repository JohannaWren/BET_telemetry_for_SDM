## 1. load libraries
library(sf)
library(raster)
require("rgdal")
require("rgeos")
require("dplyr")
library(ggExtra)
library(maptools)
library(adehabitatHR)
library(here)
library(maps)
library(glue)
library(tidyverse)

## 2. source pseudo-absence functions
source(here("C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/R/generate_pseudo_absences","create_bkgd_pseudo_absences_function.R"))

## 3. load world data to get coordinate system
data=maps::map("world2",fill=T)
IDs <- sapply(strsplit(data$names, ":"), function(x) x[1])
wrld_simpl <- map2SpatialPolygons(data, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
wrld=SpatialPolygons(wrld_simpl@polygons,proj4string=wrld_simpl@proj4string) %>%
  gBuffer(., byid=TRUE, width=0)

## 4. load global template
# this is an empty global raster at .25 degrees that we will use to ensure pseduo absences are not generated for the same day/pixel as presences
template=raster(here("C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/R/utilities","template.grd"))
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
csvdir="C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/R/generate_pseudo_absences"
polydir="C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/R/generate_pseudo_absences"
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


#####################################################################################################################################################
#####################################################################################################################################################


#try with one of my bsh files 

bsh = read_csv("C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/R/03P0282-16-GPE3.csv", skip=5)

summary(bsh) #check lat/longs etc.

#re-arrange dataframe to proper order 
#col 'tagID'
#column 'date' -> YYYY-MM-DD in *character* format (will likely need to be reformatted from original data)
#column 'lon' -> 0 to 360 longitude column in numeric format (will likely need to be transformed from -180 to 180 formated in original data)
#column 'lat' -> -90 to 90 latitude column in numeric format (will likely be in correct format in original data)


#change names of lat.long and date columns
names(bsh)[names(bsh)=="Most Likely Latitude"]<-"lat"
names(bsh)[names(bsh)=="Most Likely Longitude"]<-"lon"
names(bsh)[names(bsh)=="Date"]<-"date"
names(bsh)[names(bsh)=="DeployID"]<-"tagID"

#bsh$DeployID = as.factor(bsh$DeployID)

#change 'Date.Time.UTC' column to  to proper datetime

bsh$date <- as.POSIXct(strptime(as.character(bsh$date),format="%d-%b-%Y %H:%M:%S"))
bsh$date = as.character(bsh$date)
bsh$tagID = as.character(bsh$tagID)


#convert lat and lon

bsh$lon[bsh$lon < 0] <- bsh$lon[bsh$lon < 0] + 360 

#need to be this format AND EXACT SAME NAMES
df=data.frame(lon=as.numeric(lon),
              lat=as.numeric(lat),
              date=as.character(date),
              size=as.character(othervar1),
              tagID=as.character(othervar2),
              stringsAsFactors = F)

#select cols you want - HAS TO 

bsh = bsh %>%
  dplyr::select(lat, lon, date, tagID)

## 6. run the function
csvdir="C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/R/generate_pseudo_absences"
polydir="C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/R/generate_pseudo_absences"
species="bsh"
generate_pseudo_abs_fcn(dat=bsh,
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
## TRYING TO READ IN .CSVS WITH TEXT 'GPE3' FROM DIFFERENT FILES


##regex - Final data/Perfect_data
files_to_read = list.files(
  path = "C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/SWFSC/Final data/GPE3_csvs",   # directory to search within
  pattern = ".*(GPE3).*csv$", # regex pattern, some explanation below
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)

data_lst = lapply(files_to_read, read.csv, skip=5)  # read all the matching files

#MAKE LIST INTO ONE BIG DATAFRAME

library(data.table)
 
bsh.tracks = rbindlist(data_lst) #very good function to combine dataframes from lists 

#change date column name to date.time
names(bsh.tracks)[names(bsh.tracks)=="Date"]<-"Date.Time"

#CODE TO PARSE ALL DATES TO SAME FORMAT!
bsh.tracks$Date.Time = parse_date_time(bsh.tracks$Date.Time, orders = c('%d-%b-%Y %H:%M:%S', '%d/%m/%Y %H:%M'))

#make a date only col

bsh.tracks$Date = as.Date(bsh.tracks$Date.Time)

#change names of lat.long and date columns
names(bsh.tracks)[names(bsh.tracks)=="Most.Likely.Latitude"]<-"lat"
names(bsh.tracks)[names(bsh.tracks)=="Most.Likely.Longitude"]<-"lon"
names(bsh.tracks)[names(bsh.tracks)=="Date"]<-"date"
names(bsh.tracks)[names(bsh.tracks)=="DeployID"]<-"tagID"

#select cols to keep 

bsh.tracks = bsh.tracks %>%
  dplyr::select(tagID, lat, lon, date, Observation.Type)

#for the  SDMs the SWFSC want only one popsition per day so we are going to choose our one position based on the 'observation type'
#we want to create a nested if-else statement to return lat longs based on observation types in this order (of most to least accurate)

#User - Argos3-  Argos2 - Argos1- Argos0- Light Dawn - Light Dusk - SST - None i.e. select the lat/long for a day in this order

#check what types of observation type exist in your dataframe. Whatever the categories are, change the order of preferece in order_pref L226 below
with(bsh.tracks, table(Observation.Type))


#make df of unique tag_dates
bsh.tracks$tag_dt <- paste(bsh.tracks$tagID, bsh.tracks$date, sep="_")

out_df <- tibble(tg_dt = unique(bsh.tracks$tag_dt))

# probably an easier way to do this, but this works
out_df$tagID <- sapply(out_df$tg_dt, 
                       function(x){strsplit(x, "_")[[1]][1]})

out_df$dt    <- as.Date(sapply(out_df$tg_dt, 
                               function(x){strsplit(x, "_")[[1]][2]}))

# make dates typ Date and factor as character
bsh.tracks$date             <- as.Date(bsh.tracks$date)
bsh.tracks$Observation.Type <- as.character(bsh.tracks$Observation.Type)

# order preference
order_pref <- c( "User","Argos-3", "Argos-2", "Argos-1", "Argos-0", "Light - Dawn", "Light - Dusk",
                 "SST", "None")

# set up placeholders
out_df$lat              <- NA
out_df$lon              <- NA
out_df$Observation.Type <- NA

# set up if else for values
for(i in 1:nrow(out_df)){
  
  #get dat and tag for row
  tag_i = out_df$tagID[i]
  dt_i  = out_df$dt[i]
  
  # get data that matters
  tag_dt_i <- bsh.tracks[bsh.tracks$tagID == tag_i & 
                          bsh.tracks$date == dt_i, ]
  
  # fill in value
  for(pref_j in order_pref){
    
    # check if still NA
    if(is.na(out_df$lat[i])){
      
      # if so, check if we have any pref_j, then fill in
      if(sum(tag_dt_i$Observation.Type == pref_j) > 0){
        
        # just snag first value if there are multiple
        out_df$lat[i] <- tag_dt_i$lat[tag_dt_i$Observation.Type == pref_j][1]
        out_df$lon[i] <- tag_dt_i$lon[tag_dt_i$Observation.Type == pref_j][1]
        
        # add in obs type
        out_df$Observation.Type[i] <- 
          tag_dt_i$Observation.Type[tag_dt_i$Observation.Type == pref_j][1]
        
        
      }
    }
  }
}

head(out_df)

#change name of dt to 'date' to align with exact format required

names(out_df)[names(out_df)=="dt"]<-"date"

#change date to character for function

out_df$date = as.character(out_df$date)

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

bsh.tracks = out_df %>%
  dplyr::select(lon,lat,date,tagID)

#date needs to 

#bsh$DeployID = as.factor(bsh$DeployID)

#convert lat and lon - very important step!

bsh.tracks$lon[bsh.tracks$lon < 0] <- bsh.tracks$lon[bsh.tracks$lon < 0] + 360 

bsh.tracks$tagID = as.factor(bsh.tracks$tagID)

#create pseudo-absences ** 

## 6. run the function
csvdir="C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/R/generate_pseudo_absences"
polydir="C:/Users/Molly/Documents/NOAA/Mel_Molly Projects/Climate Change Modelling/R/generate_pseudo_absences"
species="bsh"
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


#looks like some presence go over land?!
ggplot()+
  geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
  geom_point(data=PA_bsh_all,aes(x=lon,y=lat,color=as.factor(presAbs),group=presAbs),size = 0.5, shape = 21)+
  geom_sf(data=poly,fill=NA,color="black")+
  scale_color_manual("PresAbs",values=c("1"="blue","0"="red"))+
  coord_sf(xlim = c(minx, maxx), ylim = c(miny,maxy))+
  ggtitle(glue("{species} npre={nrow(PA_bsh_all)} (1:1 ratio)"))






























