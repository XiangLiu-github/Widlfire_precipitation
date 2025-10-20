source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

inventory = read_fwf('/Volumes/External/GHCNM/ghcn-m_v4_prcp_inventory.txt') %>% 
  `names<-`(c('station', 'latitude', 'longitude', 'elevation', 'state', 'name', 'WMO_ID', 'start_year', 'end_year')) 
# filter(str_detect(station, 'GME'))

# growing_month = qread('data/outputs/growing_month.qs') %>% 
#   rast(crs = "EPSG:4326")

shp <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>% 
  filter(admin != 'Antarctica')

selected = inventory %>% 
  # filter(start_year <= 1970, end_year >= 1970) %>% 
  # mutate(start_year = fifelse(start_year <= 1970, 1970, start_year)) %>% 
  filter(longitude %between% c(-180, -50),
         latitude %between% c(20, 90))

selected %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(shp), remove = F) %>% 
  ggplot() +
  geom_sf() +
  geom_sf(aes(color = end_year - start_year)) +
  scale_color_continuous(name = element_blank())

# using read_csv has some problems
# plan(multisession, workers = 4)
# 
# a = selected %>% 
#   mutate(data = future_map(station, safely(function(astn){
#     
#     read_csv(str_c('/Users/xiangliu/Desktop/ghcn-m_v4.00.00_prcp_s16970101_e20231130_c20231204/', astn, '.csv'),
#              # pipe(str_c("tar -xOzf /Users/xiangliu/Desktop/ghcn-m_v4.00.00_prcp_s16970101_e20231130_c20231204.tar.gz ", astn, ".csv")), 
#              col_names = c('station', 'name', 'Latitude', 'Longitude', 'Elevation', 'date', 'value', 'Measurement_flag', 'Quality_flag', 'Source_flag', 'Source_index'),
#              col_select = c('date', 'value'),
#              show_col_types = F) %>% 
#       mutate(across(c(date, value), as.double))
#     
#     
#   }), .progress = T))
# 
# plan(sequential)
# 
# b = a %>% 
#   filter(map_lgl(map(a$data, 'error'), is.null)) %>% 
#   unnest_wider(data) %>% 
#   select(-error) %>% 
#   unnest()
# 
# c = a %>% 
#   filter(!map_lgl(map(a$data, 'error'), is.null)) %>% 
#   mutate(data = map(station, function(astn){
#     
#     read_csv(str_c('/Users/xiangliu/Desktop/ghcn-m_v4.00.00_prcp_s16970101_e20231130_c20231204/', astn, '.csv'),
#              col_names = c('station', 'name1', 'name2', 'Latitude', 'Longitude', 'Elevation', 'date', 'value', 'Measurement_flag', 'Quality_flag', 'Source_flag', 'Source_index'),
#              col_select = c('date', 'value'),
#              show_col_types = F) %>% 
#       mutate(across(c(date, value), as.double))
#     
#   }, .progress = T)) %>% 
#   unnest()


plan(multisession, workers = 4)

a = selected %>% 
  # head() %>% 
  mutate(data = future_map(station, safely(function(astn){
    
    fread(str_c('/Volumes/External/GHCNM/ghcn-m_v4.00.00_prcp_s16970101_e20250331_c20250405/', astn, '.csv'), col.names = c('station', 'name', 'Latitude', 'Longitude', 'Elevation', 'date', 'value', 'Measurement_flag', 'Quality_flag', 'Source_flag', 'Source_index')) %>%
      mutate(across(c(date, value), as.double),
             value = value * 0.1,
             .keep = 'used')
    
    # unit is tenths of a millimeter -> 0.1 mm -> multiply 0.1 to mm
    
  }), .progress = T))

plan(sequential)

b = a %>% 
  filter(map_lgl(map(a$data, 'error'), is.null)) %>% 
  unnest_wider(data) %>% 
  select(-error) %>% 
  unnest() %>% 
  filter(value >= 0)

b %>% 
  mutate(date = ym(date),
         year = year(date),
         month = month(date)) %>% 
  select(station, latitude, longitude, year, month, value) %>% 
  qsave('data/outputs/prep_station_monthly.qs')



