source('scripts/loadpackages.R')
source('scripts/loadfunctions.R')

prep = qread('data/outputs/prep_station_monthly.qs') %>% 
  filter(year >= 1990) 

fire_buffered = qread('data/outputs/fire_buffer_0.25.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

fire_buffered_small = qread('data/outputs/fire_buffer_0.5.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

fire_buffered_large = qread('data/outputs/fire_buffer_5.qs') %>% 
  inner_join(qread('data/outputs/fire_lc.qs')) %>% 
  filter(!lc_name %in% c("Croplands", 'Water bodies', 'Urban and Built-up Lands')) %>% 
  select(- ig_date) 

prep_station = prep %>% 
  distinct(station, longitude, latitude) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(fire_buffered), remove = F)

circle1 = prep_station %>% 
  st_intersection(fire_buffered) %>% 
  st_drop_geometry()

circle2 = prep_station %>% 
  st_intersection(fire_buffered_small) %>% 
  st_drop_geometry() %>% 
  anti_join(prep_station %>% st_intersection(fire_buffered))

circle3 = prep_station %>% 
  st_intersection(fire_buffered_large) %>% 
  st_drop_geometry() %>% 
  anti_join(prep_station %>% st_intersection(fire_buffered_small))

coefs = map_dfr(list('F = 0.25' = circle1, 'F = 0.25 ~ F = 0.5' = circle2), function(acircle){
  
  treatment = acircle
  
  treatment = treatment %>%
    distinct(station, id, tot_ar_km2) %>% 
    slice_max(tot_ar_km2, n = 1, by = c(station)) %>% 
    inner_join(treatment)
  
  control = circle3
  
  # exclude sites without adequate observation
  prep_info = prep %>%
    summarise(start_prep_year = min(year), end_prep_year = max(year), n_prep = n(), .by = c(station, latitude, longitude))
  
  treatment = treatment %>%
    inner_join(prep_info) %>%
    filter(n_prep >= 20 * 12)
  
  control = control %>%
    inner_join(prep_info) %>%
    filter(n_prep >= 20 * 12)
  
  # both control and treatment? -> possible
  inner_join(control %>% distinct(station),
             treatment %>% distinct(station))
  
  control = control %>%
    anti_join(treatment %>% distinct(station)) %>% # drop control station shown in treatment
    inner_join(treatment %>% distinct(id)) # drop id without treatment
  
  treatment = treatment %>%
    inner_join(control %>% distinct(id)) # drop id without control
  
  treatment %>% count(id)
  control %>% count(id)
  
  # also possible that one site experience two or more fires
  # this possibility can be space and time
  
  fnl = bind_rows(
    treatment %>% mutate(type = 'treatment'),
    control %>% mutate(type = 'control')
  ) %>% 
    mutate(last_date = ymd_hms(last_date),
           ig_date = fifelse(day(last_date) >= 15, last_date, last_date %m-% months(1)))
  
  # qsave(fnl, 'data/outputs/fnl_station.qs')
  
  fnl = fnl %>% 
    inner_join(prep) %>% 
    mutate(date = cal_mon_diff(ym(str_c(year, '-', month))),
           ig_date = cal_mon_diff(ig_date)) %>% 
    mutate(time_to_ig = date - ig_date) 
  
  # only include station that has -12 to zero to 12 months
  fnl = fnl %>% 
    filter(time_to_ig %in% -24:24) %>% 
    filter(n() >= 40, .by = c(station, id)) %>% 
    distinct(station, id, type) %>% 
    inner_join(fnl)
  
  # make usre to contain both treatment and control
  fnl = fnl %>%
    count(id, type) %>% 
    filter(n() == 2, .by = id) %>% 
    inner_join(fnl)
  
  mdata = bind_rows(
    fnl %>% filter(type == 'treatment'),
    fnl %>% filter(type == 'control') %>% 
      distinct(station, longitude, latitude, type) %>% 
      inner_join(prep) %>% 
      mutate(date = cal_mon_diff(ym(str_c(year, '-', month))))
  ) 
  
  mdata = mdata %>% 
    # filter(latitude %between% c(50, 70)) %>%
    mutate(ig_date = replace_na(ig_date, 10000),
           time_to_ig = replace_na(time_to_ig, -1000))
  
  mdata = mdata %>% 
    mutate(D = fifelse(time_to_ig >= 0, 1, 0))
  
  timerange = range(unique(mdata$time_to_ig[mdata$time_to_ig != -1000]))
  
  res = fepois(value ~ i(time_to_ig, ref = c('-17', -1000), bin = list('-17' = timerange[1]:-61, '-16' = -60:-49, '-15' = -48:-37, '-14' = -36:-25, '-13' = -24:-13, '16' = 49:60, '15' = 37:48, '14' = 25:36, '13' = 13:24,'17' = 61:timerange[2])) | station^month + station[year + year ^ 2] + year^month, mdata, vcov = ~ station)
  
  tidy(res)
  
}, .id = 'var')

p2 = coefs %>% 
  filter(str_detect(term, 'time_to_ig')) %>% 
  add_row(var = c('F = 0.25', 'F = 0.25 ~ F = 0.5'), term = '-17', estimate = 0, std.error = 0) %>%
  mutate(term = parse_number(term)) %>% 
  ggplot(aes(x = term, y = expm1(estimate) * 100, ymax = expm1(estimate + 1.96 * std.error)  * 100, ymin = expm1(estimate - 1.96 * std.error)  * 100, color = var)) +
  geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey60') +
  geom_vline(xintercept = c(-16.5, -15.5, -14.5, -13.5, -12.5, -0.5, 0.5, 12.5, 13.5, 14.5, 15.5, 16.5), linetype = 'longdash', color = 'grey60') +
  geom_point(position = position_dodge(0.8), size = 2) +
  geom_errorbar(position = position_dodge(0.8), width = 0.3) +
  scale_x_continuous(name = 'Time after fire', breaks = -17:17, labels = c("Prior", "-5th", "-4th", "-3rd", "-2nd", -12:12, "2nd", "3rd", "4th", "5th", "Beyond"), expand = c(0, 0.1)) +
  scale_y_continuous(name = 'Change in precipitation (%)') +
  scale_color_manual(values = c(color2, color1, color3)) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = 'inside',
        legend.position.inside = c(0.7, 0.2))

ggsave('figures/fig2_in_out_comparison.pdf', p2, width = 5 / 2, height = 4/3, scale = 3.85)
