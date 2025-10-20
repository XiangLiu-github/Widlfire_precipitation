source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

shp = rnaturalearth::ne_countries('small') %>% 
  select(admin) %>% 
  filter(admin %in% c('Canada', 'United States of America')) %>% 
  rename(country = admin)

eco = qread('data/outputs/ecoregions.qs')

fire = map2_dfr(c('canada', 'conus_ak'), c('Canada', 'United States of America'), function(aitem, acountry){
  
  afire = st_read(str_c('data/inputs/', aitem, '/fired_', aitem, '_2000_to_2024_events.gpkg')) %>% 
    select(-starts_with('eco'), -starts_with('lc'), -id) %>% 
    filter(tot_ar_km2 >= 10) %>% 
    st_transform(st_crs(shp)) 
  
  # remove duplicate fire acorss countries
  afire = st_intersection(afire, shp) %>% 
    filter(country == acountry) %>% 
    tibble() %>% 
    st_as_sf(sf_column_name = 'geom') %>% 
    rownames_to_column('id')
  
  afire
  
}) %>% 
  mutate(id = str_c(country, '_', id))

# only two fire is weird because they are include in both canada and usa
fire = fire %>% 
  st_drop_geometry() %>% 
  filter(n() == 1, .by = c(ig_date, last_date, event_dur, tot_ar_km2, x, y)) %>% 
  inner_join(fire, ., )

centroid = st_centroid(fire) %>%
  mutate(centroid_x = st_coordinates(geom)[,1],
         centroid_y = st_coordinates(geom)[,2])

centroid = st_intersection(centroid, eco) %>%
  st_drop_geometry()

fire = inner_join(fire, centroid)

# some fires have two eco name
fire = fire %>% 
  st_drop_geometry() %>% 
  filter(n() == 1, .by = c(id, ig_date, last_date, event_dur, tot_ar_km2, x, y)) %>% 
  inner_join(fire, .)

walk(c(0, 0.25, 0.5, 2, 5), function(asize){
  
  if (asize == 0) {
    fire %>% 
      qsave(str_c('data/outputs/fire_buffer_', asize, '.qs'))
  } else {
    fire %>% 
      mutate(geom = st_buffer(geom, sqrt(tot_ar_km2) / 111.32 / cos(centroid_y / 180 * pi) * asize)) %>% 
      qsave(str_c('data/outputs/fire_buffer_', asize, '.qs'))
  }
  
}, .progress = T)



















