source(here("03_analysis", "analysis_funs.r"))

# Load raw data ----
df_jp_raw <- read.csv(here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932")
df_de_raw <- read.csv(here("01_data", "intermediate", "german", "foreign_master.csv"))

df_jp_native_raw <- read.csv(here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932")
df_de_native <- read.csv(here("01_data", "intermediate", "german", "native_master.csv")) |>
  select(-total_foreign) |>
  rename(total = population)


# Data adjust ----
df_jp <- select_cols_jp(df_jp_raw)
df_de <- select_cols_de(df_de_raw)
df_jp_native <- select_cols_jp(df_jp_native_raw)

list_years <- c(2014, 2015, 2017, 2018, 2019)

# Analysis----

generate_nw_df <- function(df_input, year_i) {

  print(year_i)

  df_analysis <- df_input |>
    generate_df_main(total) |>
    dplyr::filter(year == year_i) |>
    dplyr::filter(!is.infinite(lag_ln_total))

}

generate_x_grid <- function(year_i, df_input) {

  df <- generate_nw_df(df_input, year_i)

  x_grid <- seq(min(df$lag_ln_total), max(df$lag_ln_total), length.out = 200)

  return(x_grid)

}


run_ksmooth <- function(year_i, df_input, list_x_grid) {

  df_analysis <- df_input |>
    generate_nw_df(year_i)

  mean_growth_rate <- mean(df_analysis$ln_change_rate_total, na.rm = TRUE)
  sd_growth_rate <- sd(df_analysis$ln_change_rate_total, na.rm = TRUE)

  df_analysis <- df_analysis |>
    mutate(standard_y = (ln_change_rate_total - mean_growth_rate) / sd_growth_rate)

  ks_fit <- ksmooth(
    df_analysis$lag_ln_total, 
    df_analysis$standard_y, 
    kernel = "normal", 
    bandwidth = 0.5,
    x.points = list_x_grid[[as.character(year_i)]]
    )

  # ksmoothの結果をデータフレームに変換
  df_fit <- data.frame(x = ks_fit$x, y = ks_fit$y) |>
    mutate(year = year_i)
}

run_ksmooth_bootstrap <- function(i, dataframe_i, list_x_grid, year_i) {

  df_analysis <- dataframe_i |>
    as.data.frame()

  mean_growth_rate <- mean(df_analysis$ln_change_rate_total, na.rm = TRUE)
  sd_growth_rate <- sd(df_analysis$ln_change_rate_total, na.rm = TRUE)

  df_analysis <- df_analysis |>
    mutate(standard_y = (ln_change_rate_total - mean_growth_rate) / sd_growth_rate)

  ks_fit <- ksmooth(
    df_analysis$lag_ln_total,
    df_analysis$standard_y,
    kernel = "normal",
    bandwidth = 0.5,
    x.points = list_x_grid[[as.character(year_i)]]
    )

  # ksmoothの結果をデータフレームに変換
  df_fit <- data.frame(x = ks_fit$x, y = ks_fit$y) |>
    mutate(
      year = year_i,
      bootstrap = paste0("bootstrap_", i),
      x = round(x, 3)
      )
}


# estimates bootstrap resuls
wrap_bootstrap <- function(year_i, df_input, list_x_grid) {

  df_resample <- generate_nw_df(df_input, year_i) |>
    rsample::bootstraps(times = 10)
  list_dfs <- map(df_resample$splits, rsample::analysis)

  results_bs <- purrr::map2_dfr(1:length(list_dfs), list_dfs, run_ksmooth_bootstrap, list_x_grid, year_i)

}

list_x_grid_de <- purrr::map(list_years, generate_x_grid, df_de)
names(list_x_grid_de) <- list_years

list_x_grid_jp <- purrr::map(list_years, generate_x_grid, df_jp)
names(list_x_grid_jp) <- list_years


# Original results
df_nw_de <- purrr::map_dfr(list_years, run_ksmooth, df_de, list_x_grid_de)
df_nw_jp <- purrr::map_dfr(list_years, run_ksmooth, df_jp, list_x_grid_jp)

# Bootstrap results
# df_resample <- generate_nw_df(df_de, 2014) |>
#   rsample::bootstraps(times = 10)
# list_df_2014 <- map(df_resample$splits, rsample::analysis)

# results_bs <- purrr::map2_dfr(1:length(list_df_2014), list_df_2014, run_ksmooth_bootstrap, list_x_grid, year_i = 2014)

df_bootstrap_de <- purrr::map_dfr(list_years, wrap_bootstrap, df_de, list_x_grid_de)
df_bootstrap_jp <- purrr::map_dfr(list_years, wrap_bootstrap, df_jp, list_x_grid_jp)

# Extract 5% and 95% quantiles for each x and year
df_bootstrap_quantiles_de <- df_bootstrap_de |>
  summarise(
    y_05 = quantile(y, 0.05, na.rm = TRUE),
    y_95 = quantile(y, 0.95, na.rm = TRUE),
    .by = c("x", "year")
  )

df_bootstrap_quantiles_jp <- df_bootstrap_jp |>
  summarise(
    y_05 = quantile(y, 0.05, na.rm = TRUE),
    y_95 = quantile(y, 0.95, na.rm = TRUE),
    .by = c("x", "year")
  )

# Plot ----
# Germany
plot_nw_de <- create_nw_estimate(
  df_nw_de,
  df_bootstrap_quantiles_de,
  "Non-linear estimates of German Foreigners"
)

ggsave(plot_nw_de, filename = here("04_output", "source", "figures", "nw_de.png"), width = 12, height = 8)

# Japan
plot_nw_jp <- create_nw_estimate(
  df_nw_jp, 
  df_bootstrap_quantiles_jp,
  "Non-linear estimates of Japanese Foreigners"
)

ggsave(plot_nw_jp, filename = here("04_output", "source", "figures", "nw_jp.png"), width = 12, height = 8)
