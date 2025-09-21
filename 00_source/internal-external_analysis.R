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


# Analysis ----
df_de_int_ext <- df_de |>
  add_int_ext_change() |> 
  dplyr::filter(between(year, 2014, 2019), year != 2016)

df_jp_int_ext <- df_jp |>
  add_int_ext_change() |>
  dplyr::filter(between(year, 2014, 2019), year != 2016)

plot_internal_de <- df_de_int_ext |> 
  standize_var_y(ln_change_rate_internal) |> 
  create_scatter_int(lag_ln_total, ln_change_rate_internal, 
                     title_i = "Germany Internal Migration")
  
plot_external_de <- df_de_int_ext |> 
standize_var_y(ln_change_rate_external) |>
  create_scatter_int(lag_ln_total, ln_change_rate_external, 
                     title_i = "Germany External Migration")

plot_internal_jp <- df_jp_int_ext |> 
  standize_var_y(ln_change_rate_internal) |> 
  create_scatter_int(lag_ln_total, ln_change_rate_internal,
                     title_i = "Japan External Migration")

plot_external_jp <- df_jp_int_ext |> 
  standize_var_y(ln_change_rate_external) |>
  create_scatter_int(lag_ln_total, ln_change_rate_external,
                     title_i = "Japan Internal Migration")

plot_internal_de + 
plot_external_de +
plot_internal_jp +
plot_external_jp

# lag 6 years
df_de_int_ext_lag6 <- df_de |>
  add_int_ext_change(lag_i = 6) |>
  mutate(country = "Germany") |>
  pivot_longer(
    cols = starts_with("ln_change_rate_"), 
    names_to = "migration_type", 
    values_to = "ln_change_rate"
  )

df_jp_int_ext_lag6 <- df_jp |>
  add_int_ext_change(lag_i = 6) |>
  mutate(country = "Japan") |>
  pivot_longer(
    cols = starts_with("ln_change_rate_"), 
    names_to = "migration_type", 
    values_to = "ln_change_rate"
  )

plot_de_lag6 <- df_de_int_ext_lag6 |>
  scatter_plot_lag6(
    var_x = ln_total, 
    var_y = ln_change_rate, 
    title_i = "Internal and External Migration Lag 6 Years"
  )

plot_jp_lag6 <- df_jp_int_ext_lag6 |>
  scatter_plot_lag6(
    var_x = ln_total, 
    var_y = ln_change_rate, 
    title_i = "Internal and External Migration Lag 6 Years"
  )

plot_int_ext_lag6 <- plot_de_lag6 +
  plot_jp_lag6 +
  plot_layout(
    ncol = 1,
    axis_titles = "collect"
    ) +
  plot_annotation(
    title = "Internal and External Migration Lag 6 Years",
    subtitle = "Germany and Japan") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave(plot_int_ext_lag6, filename = here("04_output", "source", "figures", "int_ext_lag6.png"), width = 12, height = 8)



df_de_int_ext_lag6 |> View()
