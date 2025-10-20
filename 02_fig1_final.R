source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

ecoregion = qread('data/outputs/ecoregions.qs')

fire = qread('data/outputs/fire_buffer_0.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

fire_buffered = qread('data/outputs/fire_buffer_0.25.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

fire_buffered_large = qread('data/outputs/fire_buffer_2.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

prep = qread('data/outputs/prep_station_monthly.qs') %>%
  filter(year >= 1990) %>% 
  filter(n() >= 12 * 20, .by = c(station, latitude, longitude))

prep_station = prep %>% 
  distinct(station, longitude, latitude) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(fire_buffered), remove = F)

p1 = ggplot() +
  geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F) +
  geom_sf(aes(fill = ig_year), data = fire, color = NA, show.legend = T) +
  scale_fill_viridis(name = 'Year of ignition',
                     breaks = c(2002, 2005, 2010, 2015, 2020, 2023),
                     labels = c(2002, 2005, 2010, 2015, 2020, 2023)) +
  scale_alpha_discrete(guide = guide_none()) +
  coord_sf(ylim = c(25, 68), xlim = c(-160, -60)) +
  theme_map() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.5, 0),
        legend.direction = "horizontal",
        legend.key.width = unit(3.5, "lines"),
        legend.key.height = unit(0.5, "lines"),
        legend.title.position = 'bottom',
        legend.title.align = 0.5,
        legend.justification = 0.5,
        legend.text.position = 'bottom')

p2 = ggplot() +
  geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F) +
  geom_sf(aes(fill = ig_year), data = fire, color = NA, show.legend = F) +
  scale_fill_viridis(name = element_blank()) +
  geom_point(aes(x = longitude, y = latitude), size = 0.2, data = prep_station) +
  coord_sf(ylim = c(33, 45), xlim = c(-125, -115)) +
  theme_map()

mdatas = qread('data/outputs/mdatas.qs')

p3 = mdatas[[5]] %>% 
  distinct(station, longitude, latitude, type) %>% 
  mutate(type = str_c(str_to_sentence(type), ' ', n()), .by = type)  %>% 
  ggplot() +
  geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F) +
  geom_point(aes(x = longitude, y = latitude, color = factor(type)), size = 0.2, position = position_jitter()) +
  scale_color_manual(name = '',
                     values = c(color2, color1)) +
  coord_sf(ylim = c(25, 68), xlim = c(-160, -60)) +
  theme_map() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.4, 0.1)) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

.id = 'United States of America_1066'

all = prep_station %>% 
  st_intersection(fire_buffered_large %>% filter(id == .id)) %>% 
  st_drop_geometry()

treatment = prep_station %>% 
  st_intersection(fire_buffered %>% filter(id == .id)) %>% 
  st_drop_geometry()

control = all %>% anti_join(treatment)

stn = bind_rows(treatment %>% mutate(type = 'treatment'),
                control %>% mutate(type = 'control'))

p4 = ggplot() +
  geom_sf(data = fire_buffered_large %>% filter(id == .id), fill = 'grey90') +
  geom_sf(data = fire_buffered %>% filter(id == .id), fill = 'grey80') +
  geom_sf(data = fire %>% filter(id == .id), fill = 'grey70') +
  geom_point(aes(x = longitude, y = latitude, color = type), data = stn, show.legend = F) +
  scale_color_manual(values = c(color2, color1)) +
  geom_text(data = data.frame(x = -116.5, y = 33.4, 
                              label = "Fire \n Perimeter"), mapping = aes(x = x, y = y, label = label), inherit.aes = FALSE, color = color1) +
  geom_text(data = data.frame(x = -116.18, y = 32.47, 
                              label = "Station"), mapping = aes(x = x, y = y, label = label), 
            inherit.aes = FALSE, color = color1) +
  geom_text(data = data.frame(x = -117.2, y = 33.36, 
                              label = "F = 0.25"), mapping = aes(x = x, y = y, label = label), 
            inherit.aes = FALSE, color = color1) +
  geom_text(data = data.frame(x = -117.69, y = 32.48, label = "F = 5"),
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = FALSE, color = color1) +
  geom_segment(data = data.frame(xend = -116.73, x = -116.59, 
                                 yend = 33.17, y = 33.28), mapping = aes(x = x, xend = xend, y = y, yend = yend), colour = color1, arrow = arrow, inherit.aes = FALSE) + # Fire perimeter
  geom_segment(data = data.frame(xend = -116.95, x = -117, 
                                 yend = 33.25, y = 33.31), mapping = aes(x = x, xend = xend, y = y, yend = yend), colour = color1, arrow = arrow, inherit.aes = FALSE) + # F = 0.25
  geom_segment(data = data.frame(xend = -117.57, x = -117.69, 
                                 yend = 32.61, y = 32.50), mapping = aes(x = x, xend = xend, y = y, yend = yend), colour = color1, arrow = arrow, inherit.aes = FALSE) + # F = 5
  geom_segment(data = data.frame(xend = -116.44, x = -116.28, 
                                 yend = 32.61, y = 32.51), mapping = aes(x = x, xend = xend, y = y, yend = yend), colour = color1, arrow = arrow, inherit.aes = FALSE) + # station
  theme_map()

p4

layout = c(
  area(1, 1, 6, 6),
  area(3, 1, 6, 2),
  area(7, 1, 12, 6),
  area(10, 1, 12, 2)
) 

plot(layout)

p = p1 + p2 + p3 + p4 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a')

ggsave('figures/fig1.pdf', p, width = 2.2, height = 1.4 * 2, scale = 5)









