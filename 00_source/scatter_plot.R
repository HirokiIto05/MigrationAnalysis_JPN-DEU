source(here("03_analysis", "analysis_funs.r"))

# Load raw data ----
df_jp_raw <- read.csv(here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932")
df_de_raw <- read.csv(here("01_data", "intermediate", "german", "foreign_master.csv"))

df_jp_native_raw <- read.csv(here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932")
df_de_native <- read.csv(here("01_data", "intermediate", "german", "native_master.csv")) |>
  select(-total_foreign) |>
  rename(total = population)


# Data ----
df_jp <- select_cols_jp(df_jp_raw)
df_de <- select_cols_de(df_de_raw)
df_jp_native <- select_cols_jp(df_jp_native_raw)

# plot ----
## Foreign population ----
plot_jp_foreign <- df_jp |>
  # generate_df_main(total, lag_i = 2) |> 
  generate_df_main_lag_n(total) |> 
  dplyr::filter(year %in% c(2015, 2019, 2023)) |>   
  create_scatter(lag_ln_total)

# ggsave(plot_jp_foreign, filename = here("04_output", "source", "figures", "jp_foreign_scatter.png"), width = 12, height = 8)
ggsave(plot_jp_foreign, filename = here("04_output", "source", "figures", "diff_lag", "jp_foreign_scatter.png"), width = 12, height = 8)

plot_de_foreign <- df_de |>
  # generate_df_main(total) |> 
  # dplyr::filter(between(year, 2014, 2023), year != 2016) |>
  generate_df_main_lag_n(total) |> 
  dplyr::filter(year %in% c(2015, 2019, 2023)) |>   
  create_scatter(lag_ln_total)

# ggsave(plot_de_foreign, filename = here("04_output", "source", "figures", "de_foreign_scatter.png"), width = 12, height = 8)
ggsave(plot_de_foreign, filename = here("04_output", "source", "figures", "diff_lag", "de_foreign_scatter.png"), width = 12, height = 8)

## Native population ----
plot_jp_native <- df_jp_native |>
  # generate_df_main(total) |> 
  # dplyr::filter(between(year, 2014, 2023)) |>
  generate_df_main_lag_n(total) |> 
  dplyr::filter(year %in% c(2015, 2019, 2023)) |>   
  create_scatter(lag_ln_total)

# ggsave(plot_jp_native, filename = here("04_output", "source", "figures", "jp_native_scatter.png"), width = 12, height = 8)
ggsave(plot_jp_native, filename = here("04_output", "source", "figures", "diff_lag", "jp_native_scatter.png"), width = 12, height = 8)

plot_de_native <- df_de_native |>
  # generate_df_main(total) |> 
  # dplyr::filter(between(year, 2014, 2023), year != 2016) |>
  generate_df_main_lag_n(total) |> 
  dplyr::filter(year %in% c(2015, 2019, 2023)) |>   
  create_scatter(lag_ln_total)

# ggsave(plot_de_native, filename = here("04_output", "source", "figures", "de_native_scatter.png"), width = 12, height = 8)
ggsave(plot_de_native, filename = here("04_output", "source", "figures", "diff_lag", "de_native_scatter.png"), width = 12, height = 8)


# Table ----

## Foreigners ----
# Japanese
table_jp_foreign <- df_jp |>
  # generate_df_main(total) |> 
  # dplyr::filter(between(year, 2014, 2019)) |>  
  generate_df_main_lag_n(total) |> 
  dplyr::filter(year %in% c(2015, 2019, 2023)) |>   
  dplyr::filter(!is.infinite(lag_ln_total)) |> 
  create_lm("ln_change_rate_total", "lag_ln_total") |>
  create_model_summary("Japanese Foreigners", "data.frame") |>
  # Select columns to remove
  select(part, term, statistic, `2015`, `2019`, `2023`)
  # dplyr::select(-`2020`,-`2021`, -`2022`)

table_jp_foreign |>
  create_kable_lm(is_save = TRUE, file_name = "jp_foreign_table")

# German
table_de_foreign <- df_de |>
  generate_df_main_lag_n(total) |> 
  dplyr::filter(year %in% c(2015, 2019, 2023)) |>   
  # generate_df_main(total) |> 
  # dplyr::filter(between(year, 2014, 2019)) |>  
  dplyr::filter(!is.infinite(lag_ln_total)) |> 
  create_lm("ln_change_rate_total", "lag_ln_total") |>
  create_model_summary("German Foreigners", "data.frame") |>
  select(part, term, statistic, `2015`, `2019`, `2023`)
  # select(-`2016`, -`2020`, -`2021`, -`2022`)


table_de_foreign |>
  create_kable_lm(is_save = TRUE, file_name = "de_foreign_table")

## Natives ----
# Japanese
table_jp_native <- df_jp_native |>
  generate_df_main_lag_n(total) |> 
  dplyr::filter(year %in% c(2015, 2019, 2023)) |>   
  # generate_df_main(total) |> 
  # dplyr::filter(between(year, 2014, 2019)) |>  
  dplyr::filter(!is.infinite(lag_ln_total)) |> 
  create_lm("ln_change_rate_total", "lag_ln_total") |>
  create_model_summary("Japanese Natives", "data.frame") |> 
  # select(-`2020`, -`2021`, -`2022`)
  select(part, term, statistic, `2015`, `2019`, `2023`)

table_jp_native |>
  create_kable_lm(is_save = TRUE, file_name = "jp_native_table")

# German
table_de_native <- df_de_native |>
  generate_df_main_lag_n(total) |> 
  dplyr::filter(year %in% c(2015, 2019, 2023)) |>   
  # generate_df_main(total) |> 
  # dplyr::filter(between(year, 2014, 2019)) |>  
  dplyr::filter(!is.infinite(lag_ln_total)) |> 
  create_lm("ln_change_rate_total", "lag_ln_total") |>
  create_model_summary("German Natives", "data.frame") |>
  # select(-`2016`, -`2020`, -`2021`, -`2022`)
  select(part, term, statistic, `2015`, `2019`, `2023`)

table_de_native |>
  create_kable_lm(is_save = TRUE, file_name = "de_native_table")


# Robustness check---- 
# German data by country
# df_de_country <- read_xlsx(here("01_data", "intermediate", "german", "country_master.xlsx")) |>
#   change_country_name()

df_de_protection <- readxl::read_xlsx(here("01_data", "intermediate", "german", "protect.xlsx")) |> 
  rename(protection = total) |>
  select(-county_name)

df_de_without_protection <- df_de |>
  left_join(df_de_protection, by = c("year", "county_id")) |>
  mutate(total = total - protection)

plot_de_foreign_without_protection <- df_de_without_protection |>  
  generate_df_main(total) |> 
  dplyr::filter(between(year, 2014, 2019), year != 2016) |>
  create_scatter(lag_ln_total)

ggsave(plot_de_foreign_without_protection, filename = here("04_output", "source", "figures", "de_foreign_without_protection_scatter.png"), width = 12, height = 8)

df_de_without_protection |>
  generate_df_main(total) |> 
  dplyr::filter(between(year, 2014, 2019)) |>  
  dplyr::filter(!is.infinite(lag_ln_total)) |> 
  create_lm("ln_change_rate_total", "lag_ln_total") |>
  create_model_summary("German Foreigners without Protection", "data.frame") |>
  select(-`2016`, -`2020`, -`2021`, -`2022`) |>
  create_kable_lm(is_save = TRUE, file_name = "de_foreign_without_protection_table")


