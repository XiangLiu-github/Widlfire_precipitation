source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

ecoregion = qread('data/outputs/ecoregions.qs')

fire_buffered = qread('data/outputs/fire_buffer_0.25.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

fire_buffered_large = qread('data/outputs/fire_buffer_5.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

prep = qread('data/outputs/prep_station_monthly.qs') %>% 
  filter(year >= 1990)

cloud = inner_join(prep, 
                   qread('data/outputs/cloud.qs')) %>% 
  select(-value)

NDVI = inner_join(prep, qread('data/outputs/NDVI.qs')) %>% 
  select(-value)

temp = qread('data/outputs/GSOM_station_monthly.qs') %>% 
  filter(year >= 1990)

SM = inner_join(prep,
                qread('data/outputs/SM.qs')) %>% 
  select(-value)

get_mdata = function(adataset){
  
  prep_station = adataset %>% 
    distinct(station, longitude, latitude) %>% 
    st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(fire_buffered), remove = F)
  
  all = prep_station %>% 
    st_intersection(fire_buffered_large) %>% 
    st_drop_geometry()
  
  treatment = prep_station %>% 
    st_intersection(fire_buffered) %>% 
    st_drop_geometry() 
  
  # strongest fire in history
  treatment = treatment %>%
    distinct(station, id, tot_ar_km2) %>% 
    slice_max(tot_ar_km2, n = 1, by = c(station)) %>% 
    inner_join(treatment)
  
  control = all %>% anti_join(treatment)
  
  # exclude sites without adequate observation
  prep_info = adataset %>%
    summarise(start_prep_year = min(year), end_prep_year = max(year), n_prep = n(), .by = c(station, latitude, longitude))
  
  treatment = treatment %>%
    inner_join(prep_info) %>%
    filter(n_prep >= 20 * 12)
  
  control = control %>%
    inner_join(prep_info) %>%
    filter(n_prep >= 20 * 12)
  
  control = control %>%
    anti_join(treatment %>% distinct(station)) %>% # drop control station shown in treatment
    inner_join(treatment %>% distinct(id)) # drop id without treatment
  
  treatment = treatment %>%
    inner_join(control %>% distinct(id)) # drop id without control
  
  fnl = bind_rows(
    treatment %>% mutate(type = 'treatment'),
    control %>% mutate(type = 'control')
  ) %>% 
    mutate(last_date = ymd_hms(last_date),
           ig_date = fifelse(day(last_date) >= 15, last_date, last_date %m-% months(1)))
  
  fnl = fnl %>% 
    inner_join(adataset) %>% 
    mutate(date = cal_mon_diff(ym(str_c(year, '-', month))),
           ig_date = cal_mon_diff(ig_date)) %>% 
    mutate(time_to_ig = date - ig_date) 
  
  fnl = fnl %>% 
    filter(time_to_ig %in% -24:24) %>% 
    filter(n() >= 40, .by = c(station, id)) %>% 
    distinct(station, id, type) %>% 
    inner_join(fnl)
  
  # make sure to contain both treatment and control
  fnl = fnl %>%
    count(id, type) %>% 
    filter(n() == 2, .by = id) %>% 
    inner_join(fnl)
  
  mdata = bind_rows(
    fnl %>% filter(type == 'treatment'),
    fnl %>% filter(type == 'control') %>% 
      distinct(station, longitude, latitude, type) %>% 
      inner_join(adataset) %>% 
      mutate(date = cal_mon_diff(ym(str_c(year, '-', month))))
  ) 
  
  mdata = mdata %>% 
    mutate(ig_date = replace_na(ig_date, 10000),
           time_to_ig = replace_na(time_to_ig, -1000))
  
  mdata = mdata %>% 
    mutate(D = fifelse(time_to_ig >= 0, 1, 0))
  
  mdata
  
}

mdatas = list(NDVI, cloud, temp, SM, prep) %>% 
  map(get_mdata)

mdatas = qread('data/outputs/mdatas.qs')

get_res_fy = function(amdata, avar, amodel){
  
  timerange = range(unique(amdata$time_to_ig[amdata$time_to_ig != -1000]))
  
  res = amodel(.[avar] ~ i(time_to_ig, ref = c('-17', -1000), bin = list('-17' = timerange[1]:-61, '-16' = -60:-49, '-15' = -48:-37, '-14' = -36:-25, '-13' = -24:-13, '16' = 49:60, '15' = 37:48, '14' = 25:36, '13' = 13:24,'17' = 61:timerange[2])) | station + station[year + year ^ 2] + year, amdata, vcov = ~ station)
  
  if (str_detect(avar, 'c\\(')) {
    res %>% 
      map_dfr(tidy, .id = 'var') %>% 
      separate(var, c('var', NA), ';')
  } else {
    tidy(res)
  }
  
}

ress = pmap(list(mdatas,
                 c('NDVI', 'c(cfc, cfc_low, cfc_middle, cfc_high)', 'c(TAVG, TMAX, TMIN)', 'sm', 'value'),
                 list(fepois, feols, feols, fepois, fepois)), get_res_fy)

p1 = ress[[1]] %>% 
  filter(str_detect(term, 'time_to_ig')) %>% 
  add_row(term = '-17', estimate = 0, std.error = 0) %>%
  mutate(term = parse_number(term)) %>% 
  ggplot(aes(x = term, y = expm1(estimate) * 100, ymax = expm1(estimate + confl * std.error)  * 100, ymin = expm1(estimate - confl * std.error)  * 100)) +
  scale_y_continuous(name = 'Change in NDVI (%)') 

p2 = ress[[2]] %>% 
  filter(str_detect(term, 'time_to_ig')) %>% 
  add_row(var = c('lhs: cfc', 'lhs: cfc_low', 'lhs: cfc_middle', 'cfc_high'), term = '-17', estimate = 0, std.error = 0) %>%
  mutate(term = parse_number(term),
         var = str_remove(var, 'lhs: ') %>% 
           factor(levels = c('cfc', 'cfc_low', 'cfc_middle', 'cfc_high'), labels = c('Total', 'Low', 'Middle', 'High'))) %>% 
  ggplot(aes(x = term, y = estimate, ymax = estimate + confl * std.error, ymin = estimate - confl * std.error, color = var)) +
  scale_y_continuous(name = 'Change in cloud fraction (%)') +
  scale_color_manual(values = c(color1, color2, color3, color4))  +
  theme_cowplot()

p3 = ress[[3]] %>% 
  filter(str_detect(term, 'time_to_ig')) %>% 
  add_row(var = c('lhs: TMAX', 'lhs: TMIN', 'lhs: TAVG'), term = '-17', estimate = 0, std.error = 0) %>%
  mutate(term = parse_number(term),
         var = str_remove(var, 'lhs: ') %>% 
           factor(levels = c('TMAX', 'TAVG', 'TMIN'))) %>% 
  ggplot(aes(x = term, y = estimate, ymax = estimate + confl * std.error, ymin = estimate - confl * std.error, color = var)) +
  scale_y_continuous(name = 'Change in temperature (°C)') + 
  scale_color_manual(values = c(color1, color3, color2))

p4 = ress[[4]] %>% 
  filter(str_detect(term, 'time_to_ig')) %>% 
  add_row(term = '-17', estimate = 0, std.error = 0) %>%
  mutate(term = parse_number(term)) %>% 
  ggplot(aes(x = term, y = expm1(estimate) * 100, ymax = expm1(estimate + confl * std.error)  * 100, ymin = expm1(estimate - confl * std.error)  * 100)) +
  scale_y_continuous(name = 'Change in soil moisture (%)')

p5 = ress[[5]] %>% 
  filter(str_detect(term, 'time_to_ig')) %>% 
  add_row(term = '-17', estimate = 0, std.error = 0) %>%
  mutate(term = parse_number(term)) %>% 
  ggplot(aes(x = term, y = expm1(estimate) * 100, ymax = expm1(estimate + confl * std.error)  * 100, ymin = expm1(estimate - confl * std.error)  * 100)) +
  scale_y_continuous(name = 'Change in precipitation (%)')

mplot = function(ap){
  
  ap +
    geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey60') +
    geom_vline(xintercept = c(-16.5, -15.5, -14.5, -13.5, -12.5, -0.5, 0.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = 'longdash', color = 'grey60') +
    geom_point(position = position_dodge(0.8), size = 2) +
    geom_errorbar(position = position_dodge(0.8), width = 0.3) +
    scale_x_continuous(name = 'Time after fire', breaks = -17:17, labels = c("Prior", "-5th", "-4th", "-3rd", "-2nd", -12:12, "2nd", "3rd", "4th", "5th", "Beyond"), expand = c(0, 0.1)) +
    theme_cowplot() +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.8),
          legend.title = element_blank(),
          legend.position = 'inside',
          legend.position.inside = c(0.7, 0.9))
  
}

fp = wrap_plots(map(list(p1, p2, p3, p4, p5), mplot), ncol = 2) +
  plot_annotation(tag_levels = 'a')

# p6 = mdatas[[1]] %>% 
#   distinct(station, longitude, latitude, type) %>% 
#   mutate(type = str_c(str_to_sentence(type), ' ', n()), .by = type)  %>% 
#   ggplot() +
#   geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F) +
#   geom_point(aes(x = longitude, y = latitude, color = factor(type)), size = 0.1, position = position_jitter()) +
#   scale_color_manual(name = '',
#                      values = c(color2, color1)) +
#   coord_sf(ylim = c(25, 68), xlim = c(-160, -60)) +
#   theme_map() +
#   theme(legend.position = 'inside',
#         legend.position.inside = c(0.8, 0.2)) +
#   guides(colour = guide_legend(override.aes = list(size = 2)))
# 
# fp[[5]] <- fp[[5]] + inset_element(p6, left = 1, bottom = 0, right = 2, top = 1, align_to = 'full')

ggsave('figures/fig2_nomonthfe.pdf', fp, width = 5, height = 4, scale = 3.85)






