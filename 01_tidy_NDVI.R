source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

station = qread('data/outputs/prep_station_monthly.qs') %>% 
  filter(year >= 1990) %>% 
  distinct(station, latitude, longitude) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326), remove = F)

NDVI <- tibble(
  files = list.files("../data_archive/PKU_GIMMS_NDVI/", full.names = T),
  files_ = list.files("../data_archive/PKU_GIMMS_NDVI/")
) %>%
  separate(files_, c(NA, NA, NA, NA, "date"), "_") %>%
  mutate(
    date = ymd(date %>% str_remove(".tif")),
    year = year(date),
    month = month(date)
  ) %>%
  select(-date) %>%
  filter(year >= 1990) %>% 
  nest(.by = c(year, month), .key = "fdata") %>%
  mutate(fdata = map2(fdata, month, function(afdata, amonth) {
    temp <- map(afdata$files, function(afile) {
      a <- rast(afile)[[1]]
      
      a[a == 65535] <- NA
      
      a <- a * 0.001
      
      return(a)
    }) %>%
      reduce(mean) %>%
      `names<-`("NDVI")
    
    terra::extract(temp, station, ID = F, bind = T) %>% 
      st_as_sf() %>% 
      st_drop_geometry()
  }, .progress = T)) %>% 
  unnest() %>% 
  replace_na()

qsave(NDVI, 'data/outputs/NDVI.qs')






