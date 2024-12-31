## creating background psuedo absences within a minimum bounding polygon of tracking data

## function to convert -180/180 to 0/360
to360 <- function(x) {x %% 360}

## function to generate pseudo-absences
## function arguments
# dat -> your presence dataframe, with lat, lon, and date columns formatted as in example (okay to have extra columns)
# csv_outdir -> where you want to save your csv with presences and pseudo-absences
# poly_outdir -> where you want to save the minimum bounding polygon that will be used to constrain where pseudo-absences are generaged
##               we want to save this because we might want to use it later to constrain model predictions so we don't innaccurately spatially extrapolate
# sp_name -> species name, no spaces, e.g. swordfish. Note: good to decide upfront 1 common name for each species that you use everytime you save a file. 
##               this will make coding a lot easier down the line

generate_pseudo_abs_fcn=function(dat,csv_outdir,poly_outdir,sp_name){
  
  ## selecting lat lon coordinates
  dat_coords=dat%>% dplyr::select(c(lon,lat))
  
  ## presences, create unique identifier for each presence based on the pixel it falls in
  dat_presences=dat %>% mutate(presAbs=1) %>% mutate(unique=raster::extract(template,dat_coords)) %>%
    mutate(unique2=glue("{date}{unique}"))
  
  ## create a minimum bounding polygon to constrain where pseudo-absences are generated
  ch <- chull(x=dat_coords$lon,y=dat_coords$lat)
  coords <- dat_coords[c(ch, ch[1]), ]
  plot(dat_coords, pch=19)
  lines(coords, col="red")
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  crs(sp_poly)=crs(wrld)
  final_poly=erase(sp_poly,wrld)
  
  ## generate pseduo-absences
  abs=spsample(final_poly, nrow(dat_coords)*2, type="random") %>% as.data.frame() ## we generate more than we need so that we can remove those in the same pixel/day as presences
  absence=dat_presences %>% rbind(dat_presences) %>% mutate(lon=abs$x,lat=abs$y)
  absence_coors=absence%>% dplyr::select(c(lon,lat))
  dat_absence=absence%>% mutate(unique=raster::extract(template,absence_coors)) %>%
    mutate(unique2=glue("{date}{unique}")) %>%
    filter(!(unique2 %in%dat_presences$unique2)) %>% .[sample(nrow(.),nrow(dat_presences)),] %>% mutate(presAbs=0)
  
  ## write out species pres / pseudo-abs dataset
  final=rbind(dat_presences,dat_absence) %>% mutate(presAbs=as.character(presAbs)) %>% arrange(presAbs)
  write.csv(final,glue("{csv_outdir}/{sp_name}_presAbs.csv"))
  
  ## write out minimum bounding polygon
  final_poly_sf=st_as_sf(final_poly)
  st_write(final_poly_sf,glue("{poly_outdir}/{sp_name}.shp"),delete_layer=T)
}