source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

mdatas = qread('data/outputs/mdatas.qs')

p1 = mdatas[[5]] %>% 
  filter(type == 'treatment') %>% 
  distinct(id, ig_month) %>% 
  ggplot(aes(x = factor(ig_month))) +
  geom_histogram(stat = "count", fill = 'grey50', color = 'black') +
  scale_x_discrete(name = 'Month') +
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

pp = p1 + p2

ggsave('figures/fire_month_area_hist.pdf', pp, width = 2, height = 1, scale = 6)

# radius
sqrt(400) * (1 + 2 * 0.25)  # 15 km
sqrt(400) * (1 + 2 * 5)  # 110 km










