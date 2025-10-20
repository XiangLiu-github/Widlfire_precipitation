# Replication Code for “Large wildfires cause local post-fire drying”

This repository contains all scripts necessary to replicate the results and figures presented in the paper **“Large wildfires cause local post-fire drying.”**

---

## 📁 File Structure and Descriptions

- **`01_*` scripts** — Data processing scripts  
  Handle all data preparation and preprocessing steps.

- **`02_*` scripts** — Figure plotting scripts  
  Generate the figures used in the paper.

- **`loadpackages.R`** — Loads all required R packages  
- **`loadfunctions.R`** — Defines custom functions used throughout the analysis  
- **`publish_figures.R`** — Organizes and exports figures in the same order as they appear in the paper

---

## ⚙️ Recommended Workflow

1. **Prepare the data**  
   - Download all datasets and place them in the appropriate folders (refer to the `01_*` scripts for directory structure).  
   - Modify directory paths directly in the scripts if needed.

2. **Run data processing scripts**  - Rscript 01_*.R
   
3. **Run plotting scripts** - Rscript 02_*.R
     
4.	**Publish figures** - Rscript publish_figures.R

⸻

📝 Notes
	•	Ensure that all required R packages are installed before running any scripts.
	•	It is recommended to source loadpackages.R and loadfunctions.R at the beginning of your R session.

---

## 📧 Contact

For questions or issues, please contact:
Xiang Liu
Email: xiangliu@fas.harvard.edu

---

### System and R packages

```r
sessioninfo::session_info()
```

```
─ Session info ────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.5.0 (2025-04-11)
 os       macOS 26.0.1
 system   aarch64, darwin20
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       America/New_York
 date     2025-10-20
 rstudio  2025.09.1+401 Cucumberleaf Sunflower (desktop)
 pandoc   3.6.3 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64/ (via rmarkdown)
 quarto   1.7.32 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto

─ Packages ────────────────────────────────────────────────────────────────
 package           * version    date (UTC) lib source
 backports           1.5.0      2024-05-23 [1] CRAN (R 4.5.0)
 bayestestR          0.17.0     2025-08-29 [1] CRAN (R 4.5.0)
 broom             * 1.0.10     2025-09-13 [1] CRAN (R 4.5.0)
 cellranger          1.1.0      2016-07-27 [1] CRAN (R 4.5.0)
 class               7.3-23     2025-01-01 [1] CRAN (R 4.5.0)
 classInt            0.4-11     2025-01-08 [1] CRAN (R 4.5.0)
 cli                 3.6.5      2025-04-23 [1] CRAN (R 4.5.0)
 codetools           0.2-20     2024-03-31 [1] CRAN (R 4.5.0)
 collapse          * 2.1.3      2025-08-18 [1] CRAN (R 4.5.0)
 correlation       * 0.8.8      2025-07-08 [1] CRAN (R 4.5.0)
 cowplot           * 1.2.0      2025-07-07 [1] CRAN (R 4.5.0)
 data.table        * 1.17.8     2025-07-10 [1] CRAN (R 4.5.0)
 datawizard          1.3.0      2025-10-11 [1] CRAN (R 4.5.0)
 DBI                 1.2.3      2024-06-02 [1] CRAN (R 4.5.0)
 dials             * 1.4.2      2025-09-04 [1] CRAN (R 4.5.0)
 DiceDesign          1.10       2023-12-07 [1] CRAN (R 4.5.0)
 digest              0.6.37     2024-08-19 [1] CRAN (R 4.5.0)
 distributional      0.5.0      2024-09-17 [1] CRAN (R 4.5.0)
 dplyr             * 1.1.4      2023-11-17 [1] CRAN (R 4.5.0)
 dreamerr            1.5.0      2025-04-18 [1] CRAN (R 4.5.0)
 e1071               1.7-16     2024-09-16 [1] CRAN (R 4.5.0)
 evaluate            1.0.5      2025-08-27 [1] CRAN (R 4.5.0)
 exactextractr     * 0.10.0     2023-09-20 [1] CRAN (R 4.5.0)
 extrafont           0.20       2025-09-24 [1] CRAN (R 4.5.0)
 extrafontdb         1.1        2025-09-28 [1] CRAN (R 4.5.0)
 farver              2.1.2      2024-05-13 [1] CRAN (R 4.5.0)
 fastmap             1.2.0      2024-05-15 [1] CRAN (R 4.5.0)
 fixest            * 0.13.2     2025-09-08 [1] CRAN (R 4.5.0)
 fontBitstreamVera   0.1.1      2017-02-01 [1] CRAN (R 4.5.0)
 fontLiberation      0.1.0      2016-10-15 [1] CRAN (R 4.5.0)
 fontquiver          0.2.1      2017-02-01 [1] CRAN (R 4.5.0)
 forcats           * 1.0.1      2025-09-25 [1] CRAN (R 4.5.0)
 Formula             1.2-5      2023-02-24 [1] CRAN (R 4.5.0)
 furrr             * 0.3.1      2022-08-15 [1] CRAN (R 4.5.0)
 future            * 1.67.0     2025-07-29 [1] CRAN (R 4.5.0)
 future.apply        1.20.0     2025-06-06 [1] CRAN (R 4.5.0)
 gdtools             0.4.4      2025-10-06 [1] CRAN (R 4.5.0)
 generics            0.1.4      2025-05-09 [1] CRAN (R 4.5.0)
 geosphere         * 1.5-20     2024-10-04 [1] CRAN (R 4.5.0)
 ggalluvial        * 0.12.5     2023-02-22 [1] CRAN (R 4.5.0)
 ggdensity         * 1.0.0      2023-02-09 [1] CRAN (R 4.5.0)
 ggdist            * 3.3.3      2025-04-23 [1] CRAN (R 4.5.0)
 ggh4x             * 0.3.1      2025-05-30 [1] CRAN (R 4.5.0)
 ggplot2           * 4.0.0      2025-09-11 [1] CRAN (R 4.5.0)
 ggpmisc           * 0.6.2      2025-07-08 [1] CRAN (R 4.5.0)
 ggpp              * 0.5.9      2025-06-28 [1] CRAN (R 4.5.0)
 ggrepel           * 0.9.6      2024-09-07 [1] CRAN (R 4.5.0)
 ggside            * 0.4.0      2025-09-13 [1] CRAN (R 4.5.0)
 ggtext            * 0.1.2      2022-09-16 [1] CRAN (R 4.5.0)
 globals             0.18.0     2025-05-08 [1] CRAN (R 4.5.0)
 glue                1.8.0      2024-09-30 [1] CRAN (R 4.5.0)
 gower               1.0.2      2024-12-17 [1] CRAN (R 4.5.0)
 GPfit               1.0-9      2025-04-12 [1] CRAN (R 4.5.0)
 gridExtra           2.3        2017-09-09 [1] CRAN (R 4.5.0)
 gridtext            0.1.5      2022-09-16 [1] CRAN (R 4.5.0)
 gtable              0.3.6      2024-10-25 [1] CRAN (R 4.5.0)
 hardhat             1.4.2      2025-08-20 [1] CRAN (R 4.5.0)
 hms                 1.1.4      2025-10-17 [1] CRAN (R 4.5.0)
 hrbrthemes        * 0.8.7      2024-03-04 [1] CRAN (R 4.5.0)
 htmltools           0.5.8.1    2024-04-04 [1] CRAN (R 4.5.0)
 infer             * 1.0.9      2025-06-26 [1] CRAN (R 4.5.0)
 insight             1.4.2      2025-09-02 [1] CRAN (R 4.5.0)
 ipred               0.9-15     2024-07-18 [1] CRAN (R 4.5.0)
 kableExtra        * 1.4.0      2024-01-24 [1] CRAN (R 4.5.0)
 KernSmooth          2.23-26    2025-01-01 [1] CRAN (R 4.5.0)
 knitr               1.50       2025-03-16 [1] CRAN (R 4.5.0)
 latex2exp         * 0.9.6      2022-11-28 [1] CRAN (R 4.5.0)
 lattice             0.22-7     2025-04-02 [1] CRAN (R 4.5.0)
 lava                1.8.1      2025-01-12 [1] CRAN (R 4.5.0)
 lhs                 1.2.0      2024-06-30 [1] CRAN (R 4.5.0)
 lifecycle           1.0.4      2023-11-07 [1] CRAN (R 4.5.0)
 listenv             0.9.1      2024-01-29 [1] CRAN (R 4.5.0)
 lubridate         * 1.9.4      2024-12-08 [1] CRAN (R 4.5.0)
 magick            * 2.9.0      2025-09-08 [1] CRAN (R 4.5.0)
 magrittr          * 2.0.4      2025-09-12 [1] CRAN (R 4.5.0)
 MASS                7.3-65     2025-02-28 [1] CRAN (R 4.5.0)
 Matrix            * 1.7-4      2025-08-28 [1] CRAN (R 4.5.0)
 MatrixModels        0.5-4      2025-03-26 [1] CRAN (R 4.5.0)
 matrixStats       * 1.5.0      2025-01-07 [1] CRAN (R 4.5.0)
 mgcv              * 1.9-3      2025-04-04 [1] CRAN (R 4.5.0)
 modeldata         * 1.5.1      2025-08-22 [1] CRAN (R 4.5.0)
 ncdf4             * 1.24       2025-03-25 [1] CRAN (R 4.5.0)
 nlme              * 3.1-168    2025-03-31 [1] CRAN (R 4.5.0)
 nnet                7.3-20     2025-01-01 [1] CRAN (R 4.5.0)
 numDeriv            2016.8-1.1 2019-06-06 [1] CRAN (R 4.5.0)
 parallelly          1.45.1     2025-07-24 [1] CRAN (R 4.5.0)
 parsnip           * 1.3.3      2025-08-31 [1] CRAN (R 4.5.0)
 patchwork         * 1.3.2      2025-08-25 [1] CRAN (R 4.5.0)
 performance       * 0.15.2     2025-10-06 [1] CRAN (R 4.5.0)
 pillar              1.11.1     2025-09-17 [1] CRAN (R 4.5.0)
 pkgconfig           2.0.3      2019-09-22 [1] CRAN (R 4.5.0)
 polynom             1.4-1      2022-04-11 [1] CRAN (R 4.5.0)
 prodlim             2025.04.28 2025-04-28 [1] CRAN (R 4.5.0)
 proxy               0.4-27     2022-06-09 [1] CRAN (R 4.5.0)
 purrr             * 1.1.0      2025-07-10 [1] CRAN (R 4.5.0)
 qs                * 0.27.3     2025-03-11 [1] CRAN (R 4.5.0)
 quantreg            6.1        2025-03-10 [1] CRAN (R 4.5.0)
 R6                  2.6.1      2025-02-15 [1] CRAN (R 4.5.0)
 RApiSerialize       0.1.4      2024-09-28 [1] CRAN (R 4.5.0)
 raster            * 3.6-32     2025-03-28 [1] CRAN (R 4.5.0)
 RColorBrewer        1.1-3      2022-04-03 [1] CRAN (R 4.5.0)
 Rcpp                1.1.0      2025-07-02 [1] CRAN (R 4.5.0)
 RcppParallel        5.1.11-1   2025-08-27 [1] CRAN (R 4.5.0)
 readr             * 2.1.5      2024-01-10 [1] CRAN (R 4.5.0)
 readxl            * 1.4.5      2025-03-07 [1] CRAN (R 4.5.0)
 recipes           * 1.3.1      2025-05-21 [1] CRAN (R 4.5.0)
 rlang               1.1.6      2025-04-11 [1] CRAN (R 4.5.0)
 rmarkdown           2.30       2025-09-28 [1] CRAN (R 4.5.0)
 rpart               4.1.24     2025-01-07 [1] CRAN (R 4.5.0)
 rsample           * 1.3.1      2025-07-29 [1] CRAN (R 4.5.0)
 rstudioapi          0.17.1     2024-10-22 [1] CRAN (R 4.5.0)
 Rttf2pt1            1.3.14     2025-09-26 [1] CRAN (R 4.5.0)
 S7                  0.2.0      2024-11-07 [1] CRAN (R 4.5.0)
 sandwich            3.1-1      2024-09-15 [1] CRAN (R 4.5.0)
 scales            * 1.4.0      2025-04-24 [1] CRAN (R 4.5.0)
 sessioninfo         1.2.3      2025-02-05 [1] CRAN (R 4.5.0)
 sf                * 1.0-21     2025-05-15 [1] CRAN (R 4.5.0)
 slider            * 0.3.2      2024-10-25 [1] CRAN (R 4.5.0)
 sp                * 2.2-0      2025-02-01 [1] CRAN (R 4.5.0)
 SparseM             1.84-2     2024-07-17 [1] CRAN (R 4.5.0)
 stringfish          0.17.0     2025-07-13 [1] CRAN (R 4.5.0)
 stringi             1.8.7      2025-03-27 [1] CRAN (R 4.5.0)
 stringmagic         1.2.0      2025-04-18 [1] CRAN (R 4.5.0)
 stringr           * 1.5.2      2025-09-08 [1] CRAN (R 4.5.0)
 survival            3.8-3      2024-12-17 [1] CRAN (R 4.5.0)
 svglite             2.2.1      2025-05-12 [1] CRAN (R 4.5.0)
 systemfonts         1.3.1      2025-10-01 [1] CRAN (R 4.5.0)
 tailor            * 0.1.0      2025-08-25 [1] CRAN (R 4.5.0)
 terra             * 1.8-70     2025-09-27 [1] CRAN (R 4.5.0)
 textshaping         1.0.4      2025-10-10 [1] CRAN (R 4.5.0)
 tibble            * 3.3.0      2025-06-08 [1] CRAN (R 4.5.0)
 tidymodels        * 1.4.1      2025-09-08 [1] CRAN (R 4.5.0)
 tidyr             * 1.3.1      2024-01-24 [1] CRAN (R 4.5.0)
 tidyselect          1.2.1      2024-03-11 [1] CRAN (R 4.5.0)
 tidyverse         * 2.0.0      2023-02-22 [1] CRAN (R 4.5.0)
 timechange          0.3.0      2024-01-18 [1] CRAN (R 4.5.0)
 timeDate            4051.111   2025-10-17 [1] CRAN (R 4.5.0)
 tune              * 2.0.1      2025-10-17 [1] CRAN (R 4.5.0)
 tzdb                0.5.0      2025-03-15 [1] CRAN (R 4.5.0)
 units               1.0-0      2025-10-09 [1] CRAN (R 4.5.0)
 usmap             * 1.0.0      2025-08-29 [1] CRAN (R 4.5.0)
 vctrs               0.6.5      2023-12-01 [1] CRAN (R 4.5.0)
 viridis           * 0.6.5      2024-01-29 [1] CRAN (R 4.5.0)
 viridisLite       * 0.4.2      2023-05-02 [1] CRAN (R 4.5.0)
 warp                0.2.1      2023-11-02 [1] CRAN (R 4.5.0)
 withr               3.0.2      2024-10-28 [1] CRAN (R 4.5.0)
 workflows         * 1.3.0      2025-08-27 [1] CRAN (R 4.5.0)
 workflowsets      * 1.1.1      2025-05-27 [1] CRAN (R 4.5.0)
 xfun                0.53       2025-08-19 [1] CRAN (R 4.5.0)
 xml2                1.4.0      2025-08-20 [1] CRAN (R 4.5.0)
 yardstick         * 1.3.2      2025-01-22 [1] CRAN (R 4.5.0)
 zoo                 1.8-14     2025-04-10 [1] CRAN (R 4.5.0)

 [1] /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library
 * ── Packages attached to the search path.

───────────────────────────────────────────────────────────────────────────
```


