source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

mdatas = qread('data/outputs/mdatas.qs')

ecoregion = qread('data/outputs/ecoregions.qs')

p1 = mdatas[[5]] %>% 
  filter(type == 'treatment') %>% 
  distinct(id, ig_month) %>% 
  ggplot(aes(x = factor(ig_month))) +
  geom_histogram(stat = "count", fill = 'grey50', color = 'black') +
  scale_x_discrete(name = 'Month',
                   labels = month.abb) +
  scale_y_continuous(name = 'Number of event', expand = c(0, 0),
                     limits = c(0, 80)) +
  theme_cowplot()

temp = mdatas[[5]] %>% 
  filter(type == 'treatment') %>% 
  distinct(id, tot_ar_km2)

p2 = temp %>% 
  ggplot(aes(x = tot_ar_km2)) +
  geom_vline(xintercept = temp %>% pull(tot_ar_km2) %>% mean(),
             linetype = 'longdash') +
  geom_histogram(bins = 40, fill = 'grey50', color = 'black') +
  scale_x_continuous(name = 'Area (km²)', 
                     expand = c(0.01, 0),
                     n.breaks = 8) +
  scale_y_continuous(name = 'Number of event', 
                     expand = c(0, 0),
                     limits = c(0, 80)) +
  theme_cowplot()

my_pal <- colorRampPalette(c("blue", "green", "yellow", "red", "orange", "blue"))(13)[1:12]

p3 = mdatas[[5]] %>% 
  filter(type == 'treatment') %>% 
  distinct(id, centroid_x, centroid_y, ig_month) %>% 
  ggplot() +
  geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F) +
  geom_point(aes(x = centroid_x, y = centroid_y, color = ig_month), size = 1) +
  scale_color_gradientn(name = 'Month',
                       colours = my_pal,
                       breaks = c(1:12),
                       labels = month.abb,
                       limits = c(1, 12),
                       na.value = 'gray60') +
  coord_sf(ylim = c(25, 68), xlim = c(-160, -60)) +
  theme_map() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.2, 0.2)) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

p4 = mdatas[[5]] %>% 
  filter(type == 'treatment') %>% 
  distinct(id, centroid_x, centroid_y, tot_ar_km2) %>% 
  mutate(tot_ar_km2 = cut(tot_ar_km2, c(0, 100, 500, 1000, 2000, 4000))) %>% 
  ggplot() +
  geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F) +
  geom_point(aes(x = centroid_x, y = centroid_y, color = tot_ar_km2), size = 1) +
  scale_color_viridis_d(name = 'Area (km²)') +
  coord_sf(ylim = c(25, 68), xlim = c(-160, -60)) +
  theme_map() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.2, 0.2)) +
  guides(colour = guide_legend(override.aes = list(size = 2)))


pp = wrap_plots(p3, p4, p1, p2, heights = c(1, 0.5))

ggsave('figures/fire_month_area_hist.pdf', pp, width = 3, height = 1.8, scale = 6)

# radius
sqrt(400) * (1 + 2 * 0.25)  # 15 km
sqrt(400) * (1 + 2 * 5)  # 110 km










