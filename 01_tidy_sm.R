source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

station = qread('data/outputs/prep_station_monthly.qs') %>% 
  filter(year >= 1990) %>% 
  distinct(station, latitude, longitude) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326), remove = F)

extracted = tibble(
  files_ = list.files('/Volumes/External/ESACCI_SM_GAPFILLED/', recursive = T),
  files = list.files('/Volumes/External/ESACCI_SM_GAPFILLED/', full.names = T, recursive = T)
) %>% 
  separate(files_, c(NA, NA, NA, NA, NA, 'date', NA), '-') %>% 
  mutate(date = ymd_hms(date),
         year = year(date),
         month = month(date)) %>% 
  nest(.by = c(year, month), .key = 'fdata') %>% 
  # head(1) %>% 
  mutate(fdata = map(fdata, function(afdata){
    
    sm = map(afdata$files, ~ rast(.x)[['sm']]) %>% rast() %>% mean() %>% `names<-`('sm')
    
    terra::extract(sm, station, ID = F, bind = T) %>% 
      st_as_sf() %>% 
      # head() %>% 
      st_drop_geometry()
    
  }, .progress = T)) %>% 
  unnest()

qsave(extracted, 'data/outputs/sm.qs')
