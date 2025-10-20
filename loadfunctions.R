
color1 = '#B6393D'
color2 = '#466D7C'
color3 = '#76894B'
color4 = '#DE9F37'

confl = qnorm(0.975)

cal_mon_diff = function(amonth){
  interval(ym('0000-01'), amonth) %/% months(1)
}

is.Raster <- function(x) {
  return((
    class(x)[1] == "RasterLayer" ||
      class(x)[1] == "RasterBrick" ||
      class(x)[1] == "RasterStack" || class(x)[1] == "SpatRaster"
  ))
}

make_fml = function(avar, abin){
  as.formula(str_c(avar, ' ~ i(time_to_ig, ref = c(-13, -1000), bin = ', as.character(constructive::construct(abin, one_liner = T))[1], ') | station^month + station[year + year ^ 2] + date'))
}

uncertainty = c(0.66, 0.95)

detrend <- function(value, year, n = 1) {
  lm(value ~ poly(year, 1), data = NULL)$residuals
}

round_pettry <- function(value) sprintf("%0.2f", round(value, digits = 2))

object_size <- function(aobject) {
  aobject %>%
    object.size() %>%
    print(unit = "auto")
}

if (Sys.info()[1] == "Windows") {
  qn <- 10
} else {
  qn <- 3
}

transform_p = function(value){
  
  if (value < 0.01) {
    "P value < 0.01"
  } else if (value < 0.05) {
    "P value < 0.05"
  } else {
    str_c("P value = ", round_pettry(value))
  }
  
  
}

arrow <-
  arrow(
    length = unit(0.015, "npc"),
    ends = "last",
    type = "open"
  )
