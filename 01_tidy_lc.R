source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

############### 500 m
fire = qread('data/outputs/fire_buffer_0.qs') %>% 
  select(id, ig_year, tot_ar_km2, centroid_x, centroid_y) 

extracted = map_dfr(2002:2023, function(ayear){
  
  lc = list.files('/Volumes/External/MCD12Q1/', str_c('MCD12Q1.A', ayear - 1, '001'), full.names = T) %>% 
    map(~ rast(.x)[[2]]) %>% # UMD
    sprc() %>% 
    merge()
  
  afire = fire %>%
    filter(ig_year == ayear) %>% 
    select(id, ig_year) %>% 
    st_transform(crs = crs(lc))
  
  res = afire %>% 
    st_drop_geometry() %>% 
    mutate(lc_code = exact_extract(lc, afire, 'mode', progress = F)) %>% 
    tibble()
  
  res
  
}, .progress = T)

qsave(extracted, 'data/outputs/landcover_intermediate.qs')

extracted = qread('data/outputs/landcover_intermediate.qs')

mapping = tibble(lc_code = c(0:10, 12:13, 15),
                 lc_name = c('Water bodies', 'Evergreen Needleleaf Forests',
                             'Evergreen Broadleaf Forests', 'Deciduous Needleleaf Forests',
                             'Deciduous Broadleaf Forests', 'Mixed Forests',
                             'Closed Shrublands', 'Open Shrublands', 'Woody Savannas', 
                             'Savannas', 'Grasslands', 'Croplands', 
                             'Urban and Built-up Lands', 'Barren'),
                 lc_type = c(NA, 'Forest', 'Forest', 'Forest', 'Forest', 
                               'Forest', 'Shrubland', 'Shrubland', 
                               'Shrubland', 'Shrubland', 'Grassland', 
                               'Cropland', NA, 'Grassland'))

lc = inner_join(extracted, mapping)

qsave(lc, 'data/outputs/fire_lc.qs')






