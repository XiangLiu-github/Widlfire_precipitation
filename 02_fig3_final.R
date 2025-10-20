source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

# read and process NDVI

NDVI <- tibble(
  files = list.files("../data_archive/PKU_GIMMS_NDVI/", full.names = T),
  files_ = list.files("../data_archive/PKU_GIMMS_NDVI/")
) %>%
  separate(files_, c(NA, NA, NA, NA, "date"), "_") %>%
  mutate(
    date = ymd(date %>% str_remove(".tif")),
    year = year(date),
    month = month(date)
  ) %>%
  select(-date) %>%
  filter(year >= 1990) %>% 
  nest(.by = c(year, month), .key = "fdata") %>%
  mutate(fdata = map2(fdata, month, function(afdata, amonth) {
    temp <- map(afdata$files, function(afile) {
      a <- rast(afile)[[1]]
      
      a[a == 65535] <- NA
      
      a <- a * 0.001
      
      return(a)
    }) %>%
      reduce(mean) %>%
      `names<-`("NDVI") %>% 
      crop(ext(c(-180, -40, 0, 90)))
    
    temp
  }, .progress = T)) 

NDVI = rast(NDVI$fdata) %>% 
  `names<-`(seq(ym('1990-01'), ym('2022-12'), 'month'))

gc()

NDVI_mean = NDVI[[1:(12*6)]] %>% 
  tapp(month(names(.)), 'mean', na.rm = T)

NDVI_ano = NDVI - NDVI_mean

# read and process cloud

cloud = list.files("../data_archive/CM_SAF/", 'CFC', full.names = T)[133:552] %>% 
  map(~ rast(.x)[['cfc']], .progress = T) %>% 
  rast() %>% 
  `names<-`(time(.))

cloud = cloud %>% 
  crop(ext(-180, -40, 0, 90))

cloud_mean = cloud[[1:(12*6)]] %>%
  # cloud_mean = cloud %>% 
  tapp(month(names(.)), 'mean', na.rm = T)

cloud_ano = cloud - cloud_mean

# read fire

fire = qread('data/outputs/fire_buffer_0.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  mutate(last_date = ymd_hms(last_date),
         ig_date = fifelse(day(last_date) >= 15, last_date, last_date %m-% months(1)) %>% format('%Y-%m') %>% ym()) 

fire = fire %>%
  inner_join(qread('data/outputs/mdatas.qs')[[5]] %>% distinct(id) %>% drop_na())

# composite

# fill gap year
NDVI_ano = c(
  NDVI_ano,
  NDVI_ano['1990'] %>% `names<-`(str_replace(names(.), '1990', '2023')) %>% `values<-`(NA),
  NDVI_ano['1990'] %>% `names<-`(str_replace(names(.), '1990', '2024')) %>% `values<-`(NA)
)

NDVI_comp = pmap(list(fire$ig_date, fire$centroid_x, fire$centroid_y, fire$geom), function(.ig_date, .centroid_x, .centroid_y, .geom){
  
  months = seq(.ig_date - months(1), 
               .ig_date + months(12), by = 'month') %>% 
    format('%Y-%m') %>% 
    str_c('-01')
  
  point = tibble(centroid_x = .centroid_x,
                 centroid_y = .centroid_y) %>% 
    st_as_sf(coords = c("centroid_x", "centroid_y"), crs = 4326) 
  
  xy = terra::extract(NDVI_ano[[1]], point, xy = T) %>% 
    select(x, y)
  
  size = 1.5 + 0.025
  
  square = rast(resolution = c(0.05, 0.05), 
                xmin = xy$x - size, xmax = xy$x + size,
                ymin = xy$y - size, ymax = xy$y + size)
  
  NDVI_c = crop(NDVI_ano, square)
  
  cons = mask(NDVI_c, vect(.geom), inverse = T) %>% 
    global('mean', na.rm = T)
  
  NDVI_c = NDVI_c - pull(cons)
  
  temp = NDVI_c[[months]] %>% 
    `names<-`(str_c('X', -1:12)) %>% 
    terra::shift(-xy$x, -xy$y)
  
  temp
  
}, .progress = T)

cloud_comp = pmap(list(fire$ig_date, fire$centroid_x, fire$centroid_y, fire$geom), function(.ig_date, .centroid_x, .centroid_y, .geom){
  
  months = seq(.ig_date - months(1), 
               .ig_date + months(12), by = 'month')
  
  months = months %>%
    format('%Y-%m') %>% 
    str_c('-01')
  
  point = tibble(centroid_x = .centroid_x,
                 centroid_y = .centroid_y) %>% 
    st_as_sf(coords = c("centroid_x", "centroid_y"), crs = 4326) 
  
  xy = terra::extract(cloud_ano[[1]], point, xy = T) %>% 
    select(x, y)
  
  size = 1.5 + 0.025
  
  square = rast(resolution = c(0.05, 0.05), 
                xmin = xy$x - size, xmax = xy$x + size,
                ymin = xy$y - size, ymax = xy$y + size)
  
  cloud_c = crop(cloud_ano, square)
  
  cons = mask(cloud_c, vect(.geom), inverse = T) %>%
    global('mean', na.rm = T)
  
  cloud_c = cloud_c - pull(cons)
  
  temp = cloud_c[[months]] %>% 
    `names<-`(str_c('X', -1:12)) %>% 
    terra::shift(-xy$x, -xy$y)
  
  return(temp)
  
}, .progress = T)

[1]

fire = fire %>% 
  mutate(NDVI_comp_ = NDVI_comp,
         cloud_comp_ = cloud_comp)

p1 = map_dfr(c("All" = "All", "Forest" = "Forest", "Shrubland" = "Shrubland", "Grassland" = "Grassland"), function(alc){
  
  if (alc == 'All') {
    subset_fire = fire
  } else {
    subset_fire = fire %>% filter(lc_type == alc)
  }
  
  a = rast(subset_fire %>% pull(NDVI_comp_))
  
  res = map(unique(names(a)), function(aname){
    
    subset(a, names(a) == aname) %>%
      mean(na.rm = T) %>%
      `names<-`(aname)
    
  }) %>% 
    rast()
  
  temp = res %>% 
    as.data.frame(xy = T) 
  
  temp
  
}, .id = 'lc_type', .progress = T) %>% 
  select(x, y, lc_type, str_c('X', -1:12)) %>% 
  pivot_longer(-c(x, y, lc_type)) %>% 
  mutate(name = factor(name, str_c('X', -1:12), c(-1:12)),
         lc_type = factor(lc_type, c('All', "Grassland", "Shrubland", "Forest"))) %>% 
  ggplot() +
  facet_grid(rows = vars(lc_type), cols = vars(name)) +
  geom_tile(aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(name = 'Change in NDVI',
                       limit = symmetric_limits(0.08), 
                       direction = 1, type = 'div',
                       oob = squish) +
  theme_cowplot() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(3, "lines"),
        legend.key.height = unit(0.5, "lines"),
        legend.title.position = 'bottom',
        legend.title.align = 0.5,
        legend.justification = 0.5,
        legend.text.position = 'bottom',
        axis.title = element_blank(),
        strip.background = element_blank())

p2 = map_dfr(c("All" = "All", "Forest" = "Forest", "Shrubland" = "Shrubland", "Grassland" = "Grassland"), function(alc){
  
  if (alc == 'All') {
    subset_fire = fire
  } else {
    subset_fire = fire %>% filter(lc_type == alc)
  }
  
  a = rast(subset_fire %>% pull(cloud_comp_))
  
  print(a)
  
  res = map(unique(names(a)), function(aname){
    
    subset(a, names(a) == aname) %>%
      mean(na.rm = T) %>%
      `names<-`(aname)
    
  }) %>%
    rast()
  
  temp = res %>% 
    as.data.frame(xy = T) %>% 
    rename_with(~ str_replace(.x, 'X\\.', 'X-'))
  
  temp
  
}, .id = 'lc_type') %>% 
  select(x, y, lc_type, str_c('X', -1:12)) %>% 
  pivot_longer(-c(x, y, lc_type)) %>% 
  mutate(name = factor(name, str_c('X', -1:12), -1:12),
         lc_type = factor(lc_type, c('All', "Grassland", "Shrubland", "Forest"))) %>% 
  ggplot() +
  facet_grid(rows = vars(lc_type), cols = vars(name)) +
  geom_tile(aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(name = 'Change in cloud fraction (%)',
                       limit = symmetric_limits(2), 
                       direction = 1, type = 'div',
                       oob = squish) +
  theme_cowplot() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(3, "lines"),
        legend.key.height = unit(0.5, "lines"),
        legend.title.position = 'bottom',
        legend.title.align = 0.5,
        legend.justification = 0.5,
        legend.text.position = 'bottom',
        axis.title = element_blank(),
        strip.background = element_blank())

fp = wrap_plots(p1, p2, design = 
                  'C
                   D') &
  scale_y_continuous(breaks = c(-1, 0, 1),
                     labels = c(-1, 0, 1)) &
  scale_x_continuous(breaks = c(-1, 0, 1),
                     labels = c(-1, 0, 1)) &
  plot_annotation(tag_levels = 'a')

ggsave('figures/fig3.pdf', fp, width = 4, height = 3, scale = 4)
