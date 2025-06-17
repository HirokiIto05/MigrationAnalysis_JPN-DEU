# Setup -----------------------------------------
source(here::here("05_config", "library.r"))


# Clean -----------------------------------------

# 「住民基本台帳」関連分析
source(here::here("02_clean", "population", "population.r")) # create panel data

# 「在留外国人統計」関連分析
source(here::here("02_clean", "foreign_status", "01_aggregate_status.r"))
source(here::here("02_clean", "foreign_status", "02_master_foreign.r"))

# Analyze ---------------------------------------

# 「住民基本台帳」関連分析
source(here::here("03_analysis", "population", "figure_5.r"))
source(here::here("03_analysis", "population", "figure_6.r"))
source(here::here("03_analysis", "population", "table_5.r"))
source(here::here("03_analysis", "population", "table_6.r"))
source(here::here("03_analysis", "population", "appendix_figure_3.r"))

# 「在留外国人統計」関連分析
source(here::here("03_analysis", "foreign_status", "figure_7.r"))
source(here::here("03_analysis", "foreign_status", "figure_8.r"))
source(here::here("03_analysis", "foreign_status", "table_7.r"))
source(here::here("03_analysis", "foreign_status", "table_8.r"))
source(here::here("03_analysis", "foreign_status", "appendix_table_3.r"))

