source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

prep = qread('data/outputs/prep_station_monthly.qs') %>% 
  filter(year >= 1990)

fire_buffered = qread('data/outputs/fire_buffer_0.25.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

mdatas = map(c('0.5' = 0.5, '2' = 2, '5' = 5), function(abuffer){
  
  fire_buffered_large = qread(str_c('data/outputs/fire_buffer_', abuffer, '.qs')) %>%
    inner_join(qread('data/outputs/fire_lc.qs')) %>% 
    filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
    select(- ig_date) 
  
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
  
  get_mdata(prep)
  
})

coefs = map_dfr(mdatas, function(amdatas){
  
  get_res_fy = function(amdata, avar, amodel){
    
    timerange = range(unique(amdata$time_to_ig[amdata$time_to_ig != -1000]))
    
    res = amodel(.[avar] ~ i(time_to_ig, ref = c('-17', -1000), bin = list('-17' = timerange[1]:-61, '-16' = -60:-49, '-15' = -48:-37, '-14' = -36:-25, '-13' = -24:-13, '16' = 49:60, '15' = 37:48, '14' = 25:36, '13' = 13:24,'17' = 61:timerange[2])) | station^month + station[year + year ^ 2] + year^month, amdata, vcov = ~ station, nthread = 0)
    
    if (str_detect(avar, 'c\\(')) {
      res %>% 
        map_dfr(tidy, .id = 'var') %>% 
        separate(var, c('var', NA), ';')
    } else {
      tidy(res)
    }
    
  }
  
  get_res_fy(amdatas, 'value', fepois)
  
}, .id = 'buffer')

coefs %>% 
  filter(str_detect(term, 'time_to_ig')) %>% 
  add_row(buffer = c('0.5', '2', '5'), term = '-17', estimate = 0, std.error = 0) %>%
  mutate(term = parse_number(term)) %>% 
  ggplot(aes(x = term, y = expm1(estimate) * 100, ymax = expm1(estimate + 1.96 * std.error)  * 100, ymin = expm1(estimate - 1.96 * std.error)  * 100, color = buffer)) + 
  geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey60') +
  geom_vline(xintercept = c(-16.5, -15.5, -14.5, -13.5, -12.5, -0.5, 0.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = 'longdash', color = 'grey60') +
  geom_point(position = position_dodge(0.8), size = 2, show.legend = T) +
  geom_errorbar(position = position_dodge(0.8), width = 0.3, show.legend = T) +
  scale_x_continuous(name = 'Time after fire', breaks = -17:17, labels = c("Prior", "-5th", "-4th", "-3rd", "-2nd", -12:12, "2nd", "3rd", "4th", "5th", "Beyond"), expand = c(0, 0.1)) +
  scale_y_continuous(name = 'Change in precipitation (%)') +
  scale_color_manual(values = c(color1, color2, color3, color4)) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.8),
        legend.title = element_blank(),
        legend.position = 'inside',
        legend.position.inside = c(0.7, 0.9)) 

ggsave('figures/fig2_0.25_changing_buffer.pdf', width = 2, height = 1, scale = 6)

#############

source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

prep = qread('data/outputs/prep_station_monthly.qs') %>% 
  filter(year >= 1990)

fire_buffered_large = qread('data/outputs/fire_buffer_5.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

mdatas = map(c('0' = 0, '0.25' = 0.25, '0.5' = 0.5, '2' = 2), function(abuffer){
  
  fire_buffered = qread(str_c('data/outputs/fire_buffer_', abuffer, '.qs')) %>%
    inner_join(qread('data/outputs/fire_lc.qs')) %>% 
    filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
    select(- ig_date) 
  
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
  
  get_mdata(prep)
  
})

coefs = map_dfr(mdatas, function(amdatas){
  
  get_res_fy = function(amdata, avar, amodel){
    
    timerange = range(unique(amdata$time_to_ig[amdata$time_to_ig != -1000]))
    
    res = amodel(.[avar] ~ i(time_to_ig, ref = c('-17', -1000), bin = list('-17' = timerange[1]:-61, '-16' = -60:-49, '-15' = -48:-37, '-14' = -36:-25, '-13' = -24:-13, '16' = 49:60, '15' = 37:48, '14' = 25:36, '13' = 13:24,'17' = 61:timerange[2])) | station^month + station[year + year ^ 2] + year^month, amdata, vcov = ~ station, nthread = 0)
    
    if (str_detect(avar, 'c\\(')) {
      res %>% 
        map_dfr(tidy, .id = 'var') %>% 
        separate(var, c('var', NA), ';')
    } else {
      tidy(res)
    }
    
  }
  
  get_res_fy(amdatas, 'value', fepois)
  
}, .id = 'buffer')

coefs %>% 
  filter(str_detect(term, 'time_to_ig')) %>% 
  add_row(buffer = c('0', '0.25', '0.5', '2'), term = '-17', estimate = 0, std.error = 0) %>%
  mutate(term = parse_number(term)) %>% 
  ggplot(aes(x = term, y = expm1(estimate) * 100, ymax = expm1(estimate + 1.96 * std.error)  * 100, ymin = expm1(estimate - 1.96 * std.error)  * 100, color = buffer)) + 
  geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey60') +
  geom_vline(xintercept = c(-16.5, -15.5, -14.5, -13.5, -12.5, -0.5, 0.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = 'longdash', color = 'grey60') +
  geom_point(position = position_dodge(0.8), size = 2, show.legend = T) +
  geom_errorbar(position = position_dodge(0.8), width = 0.3, show.legend = T) +
  scale_x_continuous(name = 'Time after fire', breaks = -17:17, labels = c("Prior", "-5th", "-4th", "-3rd", "-2nd", -12:12, "2nd", "3rd", "4th", "5th", "Beyond"), expand = c(0, 0.1)) +
  scale_y_continuous(name = 'Change in precipitation (%)') +
  scale_color_manual(values = c(color1, color2, color3, color4)) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.8),
        legend.title = element_blank(),
        legend.position = 'inside',
        legend.position.inside = c(0.7, 0.9)) 

ggsave('figures/fig2_5_changing_buffer.pdf', width = 2, height = 1, scale = 6)






