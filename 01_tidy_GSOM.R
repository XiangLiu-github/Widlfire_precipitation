source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

shp <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>% 
  filter(admin != 'Antarctica')

inventory = read_fwf('/Volumes/External/global-summary-of-the-month/ghcnd-inventory.txt') %>% 
  `names<-`(c('station', 'latitude', 'longitude', 'var', 'start', 'end')) %>% 
  # filter(var %in% c('TMAX', 'TMIN', 'TAVG')) %>%
  filter(longitude %between% c(-180, -50),
         latitude %between% c(20, 90))

inventory = inventory %>% 
  distinct(station, longitude, latitude) 

inventory %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(shp), remove = F) %>% 
  ggplot() +
  geom_sf(data = shp) +
  geom_sf()

plan(multisession, workers = 4)

tdata = inventory %>% 
  # head() %>% 
  mutate(data = future_map(station, function(astation){
    
    astation = str_c('/Volumes/External/global-summary-of-the-month/gsom-latest/', astation, '.csv')
    
    if (file.exists(astation)) {
      fread(astation) %>% 
        select(any_of(c('DATE', 'TAVG', 'TMAX', 'TMIN', 'EMXT', 'EMNT', 'PRCP', 'EMXP')))
    } 
    
  }, .progress = T)) %>% 
  unnest()

plan(sequential)

# tdata = tdata %>% 
#   drop_na(!ends_with('ATTRIBUTES')) %>% 
#   separate_wider_delim(TAVG_ATTRIBUTES, delim = ',', names = c('TAVG_num', NA)) %>% 
#   separate_wider_delim(TMAX_ATTRIBUTES, delim = ',', names = c('TMAX_num', NA, NA, NA)) %>% 
#   separate_wider_delim(TMIN_ATTRIBUTES, delim = ',', names = c('TMIN_num', NA, NA, NA)) 
  # separate_wider_delim(PRCP_ATTRIBUTES, delim = ',', names = c('PRCP_num', NA, NA, NA)) %>% 

tdata %>% summary() # looks like not too many missing data at all

tdata = tdata %>% 
  mutate(date = ym(DATE),
         year = year(date),
         month = month(date)) %>% 
  select(-DATE, -date)

tdata %>% 
  qsave('data/outputs/GSOM_station_monthly.qs')











