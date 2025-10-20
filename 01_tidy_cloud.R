source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

station = qread('data/outputs/prep_station_monthly.qs') %>% 
  filter(year >= 1990) %>% 
  distinct(station, latitude, longitude) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326), remove = F)

cloud = map_dfr(list.files("../data_archive/CM_SAF/", 'CFC', full.names = T)[133:553], function(afile){
  
  arst = rast(afile)[[c('cfc', 'cfc_low', 'cfc_middle', 'cfc_high', 'cfc_day', 'cfc_night')]] 
  
  terra::extract(arst, station, ID = F, bind = T) %>% 
    st_as_sf() %>% 
    st_drop_geometry() %>%
    mutate(year = year(time(arst)[1]),
           month = month(time(arst)[1]))
  
}, .progress = T)

qsave(cloud, 'data/outputs/cloud.qs')

