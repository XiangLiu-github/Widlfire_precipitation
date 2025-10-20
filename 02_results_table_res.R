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

get_res_fy = function(amdata, avar, amodel){
  
  timerange = range(unique(amdata$time_to_ig[amdata$time_to_ig != -1000]))
  
  res = amodel(.[avar] ~ i(time_to_ig, ref = c('-17', -1000), bin = list('-17' = timerange[1]:-61, '-5th' = -60:-49, '-4th' = -48:-37, '-3th' = -36:-25, '-2rd' = -24:-13, '5th' = 49:60, '4th' = 37:48, '3th' = 25:36, '2rd' = 13:24, 'Beyond' = 61:timerange[2])) | station^month + station[year + year ^ 2] + year^month, amdata, vcov = ~ station)
  
  res
  
}

ress = pmap(list(mdatas,
                 c('NDVI', 'c(cfc, cfc_low, cfc_middle, cfc_high)', 'c(TAVG, TMAX, TMIN)', 'sm', 'value'),
                 list(fepois, feols, feols, fepois, fepois)), get_res_fy)

resul = pmap(list(1:5,
          c('NDVI', 'cloud', 'temperature', 'soil moisture', 'precipitation'),
          ress), function(aidx, aname, adata){
            
            test = adata %>% 
              etable(title = str_c('\\label{tab:suptab', aidx, '}Regression result of dynamic DID model for ', aname, '. Note that the coefficient for prior is omitted.'),
                     dict = c('cfc' = 'Cloud fraction', 
                              'cfc_low' = 'Low cloud fraction',
                              'cfc_middle' = 'Middle cloud fraction',
                              'cfc_high' = 'High cloud fraction',
                              'sm' = 'Soil moisture',
                              'value' = 'Precipitation',
                              'time_to_ig' = 'time'),
                     tex = T, se.below = F) %>% 
              as.character()
            
            test[7] <- ''
            
            test[4] <- '\\begin{adjustbox}{height=0.5\\textheight,center}\\begin{tabular}{lccccc}'
            
            test[test == "   \\end{tabular}"] <- '\\end{tabular}\\end{adjustbox}'
            
            test
            
          }) 

writeLines(c(resul[[1]], '\\clearpage', resul[[2]], '\\clearpage', resul[[3]], '\\clearpage', resul[[4]], '\\clearpage', resul[[5]]), "paper/sup/additional_files/regrss_tbl.tex")






