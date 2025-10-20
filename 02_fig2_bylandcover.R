source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

ecoregion = qread('data/outputs/ecoregions.qs')

fire_buffered = qread('data/outputs/fire_buffer_0.25.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) %>% 
  filter(id != 'United States of America_9508')

fire_buffered_large = qread('data/outputs/fire_buffer_5.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) %>% 
  filter(id != 'United States of America_9508')

prep = qread('data/outputs/prep_station_monthly.qs') %>% 
  filter(year >= 1990,
         latitude <= 50,
         longitude <= -90)

cloud = inner_join(prep, 
                   qread('data/outputs/cloud.qs')) %>% 
  select(-value)

NDVI = inner_join(prep, qread('data/outputs/NDVI.qs')) %>% 
  select(-value)

temp = qread('data/outputs/GSOM_station_monthly.qs') %>% 
  filter(year >= 1990,
         latitude <= 50,
         longitude <= -90)

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
  
  # qsave(fnl, 'data/outputs/fnl_station.qs')
  
  fnl = fnl %>% 
    inner_join(adataset) %>% 
    mutate(date = cal_mon_diff(ym(str_c(year, '-', month))),
           ig_date = cal_mon_diff(ig_date)) %>% 
    mutate(time_to_ig = date - ig_date) 
  
  # only include station that has -12 to zero to 12 months
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

get_res = function(amdata, avar, amodel){
  
  amdata = amdata %>% 
    drop_na(all_of(avar))
  
  amdata = amdata %>% 
    mutate(lc_type = replace_na(lc_type, 'NA'))
  
  timerange = range(unique(amdata$time_to_ig[amdata$time_to_ig != -1000]))
  
  res = amodel(.[avar] ~ i(time_to_ig, ref = c('-17', -1000), bin = list('-17' = timerange[1]:-61, '-16' = -60:-49, '-15' = -48:-37, '-14' = -36:-25, '-13' = -24:-13, '16' = 49:60, '15' = 37:48, '14' = 25:36, '13' = 13:24,'17' = 61:timerange[2])) | station^month + station[year + year ^ 2] + year^month, amdata, vcov = ~ station, fixef.rm = "none")
  
  mmx = model.matrix(res)
  
  fnl_data = map_dfc(1:ncol(mmx), function(acol){
    
    i(amdata$lc_type, mmx[,acol], ref = 'NA') %>% 
      as_tibble() %>% 
      rename_with(~ str_c(str_replace(str_replace(colnames(mmx)[acol], '-', 'q'), '::', '_'), '_', .x))
    
  }) 
  
  amdata = bind_cols(amdata, fnl_data)
  
  gc()
  
  res = amodel(as.formula(str_c(avar, ' ~ ', str_c(names(fnl_data), collapse = '+'), '| station^month + station[year + year ^ 2] + date'))
               , amdata, vcov = ~ station)
  
  tidy(res)
  
}

ress = pmap(list(mdatas,
                 c('NDVI', 'cfc', 'TAVG', 'sm', 'value'),
                 list(fepois, feols, feols, fepois, fepois)), get_res)

p15 = map2(ress,
           c('Change in NDVI (%)', 'Change in cloud fraction (%)', 'Change in temperature (°C)', 'Change in soil moisture (%)',  'Change in precipitation (%)'),
           function(ares, alabel){
             
             ares = ares %>% 
               separate(term, c(NA, NA, NA, 'term', 'lc')) %>% 
               mutate(term = str_replace(term, 'q', '-') %>% parse_number()) %>% 
               add_row(lc = c('Forest', 'Shrubland', 'Grassland'), term = -17, estimate = 0, std.error = 0)
             
             if (str_detect(alabel, 'cloud|temperature')) {
               
               p = ares %>% 
                 ggplot(aes(x = term, y = estimate, ymax = estimate + confl * std.error, ymin = estimate - confl * std.error, color = factor(lc))) +
                 scale_y_continuous(name = alabel) 
               
             } else {
               
               p = ares %>% 
                 ggplot(aes(x = term, y = expm1(estimate) * 100, ymax = expm1(estimate + confl * std.error) * 100, ymin = expm1(estimate - confl * std.error) * 100, color = factor(lc))) +
                 scale_y_continuous(name = alabel) 
               
             }
             
             p + 
               geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey60') +
               geom_vline(xintercept = c(-16.5, -15.5, -14.5, -13.5, -12.5, -0.5, 0.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = 'longdash', color = 'grey60') +
               geom_point(position = position_dodge(0.8), size = 2, show.legend = F) +
               geom_errorbar(position = position_dodge(0.8), width = 0.3, show.legend = F) +
               scale_x_continuous(name = 'Time after fire', breaks = -17:17, labels = c("Prior", "-5th", "-4th", "-3rd", "-2nd", -12:12, "2nd", "3rd", "4th", "5th", "Beyond"), expand = c(0, 0.1)) +
               scale_color_manual(values = c(color1, color3, color2),
                                  breaks = c("Grassland", "Shrubland", "Forest")) +
               theme_cowplot() +
               theme(axis.text.x = element_text(angle = 30, vjust = 0.8)) 
             
           })

p6 = mdatas[[5]] %>% 
  filter(type == 'treatment') %>% 
  distinct(id, lc_type) %>% 
  mutate(lc_type = factor(lc_type, levels = c('Grassland', 'Shrubland', 'Forest'))) %>% 
  right_join(fire_buffered, .) %>%
  rename(`Land cover` = lc_type)%>% 
  ggplot() +
  geom_sf(aes(alpha = eco_name), color = 'grey70', data = ecoregion, show.legend = F) +
  geom_point(aes(x = centroid_x, y = centroid_y, color = `Land cover`), size = 1) +
  scale_color_manual(name = '',
                     values = c(color1, color3, color2),
                     breaks = c("Grassland", "Shrubland", "Forest")) +
  coord_sf(ylim = c(25, 68), xlim = c(-160, -60)) +
  theme_map() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.2, 0.2)) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

fp = wrap_plots(p15, ncol = 2) &
  plot_annotation(tag_levels = 'a')

fp[[5]] <- fp[[5]] + inset_element(p6, left = 1, bottom = 0, right = 2, top = 1, align_to = 'full')

ggsave('figures/fig2_bylandcover.pdf', fp, width = 5, height = 4, scale = 3.85)















