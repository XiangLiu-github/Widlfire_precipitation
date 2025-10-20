source('scripts/loadpackages.R')

ecoregion = qread('data/outputs/ecoregions.qs')

all = read_csv('/Volumes/External/Dheed/MergedEventStats_landonly_int.csv.zip') %>% 
  select(label, start_time, end_time) %>% 
  rename(labels = label)

eoe = list.files(path = "/Volumes/External/Dheed/", pattern = "\\.csv$", full.names = TRUE) %>% 
  map_dfr(read_csv) %>% 
  slice_max(Ti, n = 1, by = c(latitude, longitude, labels)) %>% 
  mutate(longitude = fifelse(longitude > 180, longitude - 360, longitude))

eoe = eoe %>% 
  filter(labels %in% all$labels)

eoe %>% 
  count(labels, sort = T) 

# Create a polygon around each point
cell_size <- 0.25  # 0.25 degrees
half_size <- cell_size / 2

# Function to create a polygon for one point
make_polygon <- function(lon, lat) {
  coords <- matrix(c(
    lon - half_size, lat - half_size,
    lon + half_size, lat - half_size,
    lon + half_size, lat + half_size,
    lon - half_size, lat + half_size,
    lon - half_size, lat - half_size
  ), ncol = 2, byrow = TRUE)
  
  sf::st_polygon(list(coords))
}

dheed = eoe %>% 
  select(longitude, latitude, labels) %>% 
  nest(.by = labels) %>% 
  mutate(n = map(data, nrow),
         geometry = map(data, function(adata){
    
    adata %>%
      mutate(geometry = map2(longitude, latitude, make_polygon)) %>%
      st_as_sf(crs = st_crs(ecoregion)) %>% 
      st_union() 
    
  }, .progress = T), .keep = 'unused') %>% 
  unnest() %>% 
  st_as_sf() %>% 
  inner_join(all) 

dheed = dheed %>% 
  st_intersection(st_union(ecoregion))

qsave(dheed, 'data/outputs/dheed.qs')

plot(dheed %>% slice_min(n, n = 10) %>% select(1))

eoe %>% 
  filter(labels == 73877) %>% 
  ggplot() +
  geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F,) +
  geom_tile(aes(x = longitude, y = latitude, fill = Ti)) +
  theme_map() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.8, 0.2))

dheed %>% 
  filter(labels == 73877) %>% 
  ggplot() +
  geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F,) +
  geom_sf(fill = 'red') +
  theme_map() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.8, 0.2))


# test for eu and see the match




