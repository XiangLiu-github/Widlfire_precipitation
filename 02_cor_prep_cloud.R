source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

prep = qread('data/outputs/prep_station_monthly.qs') %>% 
  filter(year >= 1990)

cloud = inner_join(prep, 
                   qread('data/outputs/cloud.qs'))

p = feols(value ~ sw(cfc, cfc_low, cfc_middle, cfc_high), cloud) %>% 
  map_dfr(fixest::r2, .id = 'type') %>% 
  mutate(type = str_remove(type, 'rhs: ') %>% 
           factor(c('cfc', 'cfc_low', 'cfc_middle', 'cfc_high'),
                  c('Total', 'Low', 'Middle', 'High'))) %>% 
  ggplot(aes(x = type, y = sqrt(cor2))) +
  geom_col(fill = 'grey50', color = 'black', width = 0.5) +
  scale_x_discrete(name = 'Cloud height') +
  scale_y_continuous(name = 'Correlation with monthly precipitation', expand = c(0, 0),
                     limits = c(0, 0.4)) +
  theme_cowplot()

ggsave('figures/cor_prep_cloud.pdf', p, width = 1, height = 1, scale = 6)

