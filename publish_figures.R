rm(list = ls())
gc()
library(tidyverse)

# remove files ------------------------------------------------------------

list.files("figures_paper/", full.names = T) %>%
  walk(file.remove)

# main --------------------------------------------------------------------

c(
  "figures/fig1.pdf",
  "figures/fig2.pdf",
  "figures/fig3.pdf"
) %>%
  iwalk(function(afig, anum) {
    file.copy(afig, str_c("figures_paper/Fig", anum, ".pdf"), overwrite = T)
  })

# supplementary ----------------------------------------------------------------

c(
  "figures/prep_cloud_cor.pdf",
  "figures/fig2_0.25_changing_buffer.pdf",
  "figures/fig2_5_changing_buffer.pdf",
  'figures/fire_month_area_hist.pdf',
  "figures/fig2_nomonthfe.pdf", 
  "figures/dheed_map.pdf", 
  "figures/fig2_in_out_comparison.pdf",
  "figures/fig2_bylandcover.pdf",
  "figures/fig2_byregion.pdf",
  "figures/fig2_seasonlong.pdf"
) %>%
  iwalk(function(afig, anum) {
    file.copy(afig, str_c("figures_paper/SIFig", anum, ".pdf"), overwrite = T)
  })



