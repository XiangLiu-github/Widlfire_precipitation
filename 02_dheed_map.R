source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

ecoregion = qread('data/outputs/ecoregions.qs')

fire_buffered = qread('data/outputs/dheed.qs') %>% 
  mutate(area = units::drop_units(st_area(.)) / 1e6) %>% 
  slice_max(area, n = 1000) %>% 
  rename(id = labels)

p = fire_buffered %>% 
  slice_max(area, n = 3) %>% 
  ggplot() +
  geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F) +
  geom_sf(aes(fill = factor(id)), show.legend = F) +
  geom_sf_text(aes(label = factor(id)), color = 'white') +
  scale_fill_manual(values = c(color1, color2, color3)) +
  coord_sf(ylim = c(25, 68), xlim = c(-160, -60)) +
  theme_map()

ggsave('figures/dheed_map.pdf', p, width = 1.8, height = 1.25, scale = 5)


