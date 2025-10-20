source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

# https://bplant.org/regions.php
# https://www.epa.gov/eco-research/ecoregions

shp = rnaturalearth::ne_countries('medium') %>% 
  filter(admin %in% c('Canada', 'United States of America')) %>% 
  st_union()

eco = st_read('data/inputs/na_cec_eco_l1/NA_CEC_Eco_Level1.shp') %>% 
  st_transform("EPSG:4326") %>% 
  filter(!NA_L1NAME %in% c('WATER', 'ARCTIC CORDILLERA', 'TUNDRA')) %>% 
  mutate(eco_name = str_to_sentence(NA_L1NAME),
         eco_name = case_when(eco_name == 'Southern semiarid highlands' ~ 'North american deserts',
                              eco_name == 'Hudson plain' ~ 'Taiga',
                              eco_name == 'Temperate sierras' ~ 'North american deserts',
                              eco_name == 'Tropical wet forests' ~ "Eastern temperate forests",
                              TRUE ~ eco_name),
         .keep = 'unused') %>% 
  summarise(geometry = st_union(geometry), .by = eco_name) %>% 
  st_simplify(dTolerance = 0.01) %>% 
  st_intersection(shp) 

eco %>% 
  ggplot() +
  geom_sf(aes(fill = eco_name), show.legend = F) +
  geom_sf_text(aes(label = eco_name), show.legend = F) +
  theme_map()

qsave(eco, 'data/outputs/ecoregions.qs')


