main <- function() {
  
  list_year <- seq(2013, 2022)

  df_de <- read_csv(here::here('01_data', 'intermediate', 'german', 'foreign_master.csv'))

  df_de <- select_cols_de(df_de)

  df_cor <- readxl::read_xlsx(here::here('01_data', 'intermediate', 'german', "nuts_correspondence.xlsx")) |>
    select(county_id, nuts_code3)

  df_geo <- read_sf(here::here(
    '01_data', 'raw', 'mapping', 'NUTS250_N3.shp')) |>
    dplyr::filter(GF == 4) |>
    select(NUTS_CODE, geometry) |>
    distinct()

  df_de_map <- df_de |>
    left_join(df_cor, by = "county_id") |>
    left_join(df_geo, by = c("nuts_code3" = "NUTS_CODE"))

  df_analysis <- generate_df_main(df_de_map, total, lag_i = 5)

  plot <- plot_tmap(2014, df_analysis, ln_change_rate_total, "ln_change_rate_total", "Change rate of total population")

  purrr::map(list_year, plot_tmap_rank, df_joint,
             ln_mvin_int, "ln_mvin_int", "From overseas",
             "from_overseas", "pdf")
  
  purrr::map(list_year, plot_tmap_rank, df_joint,
             ln_mvin_dom, "ln_mvin_dom", "From domestic",
             "from_domestic", "pdf")

  purrr::map(list_year, plot_tmap_rank, df_joint,
             ln_mvin_dom, "ln_mvin_dom", "From overseas",
             "from_overseas")

  
}


select_cols_jp <- function(df_jp) {

  df_output <- df_jp |>
    select(
      county_id = city_id, county_name = city_name, prefecture_name, year, total,
      add = moving_in_total,
      add_internal = moving_in_dom,
      add_external = moving_in_int,
      add_birth = birth,
      exit = moving_out_total,
      exit_internal = moving_out_dom,
      exit_external = moving_out_int,
      exit_death = mortality
    )
}


select_cols_de <- function(df_de) {

  df_output <- df_de |>
    select(
      county_id, county_name, year, total = total_value,
      add = additions,
      add_internal = addition_internal,
      add_external = addition_international,
      add_birth = addition_by_birth,
      exit = exits,
      exit_internal = exit_internal,
      exit_external = extis_international,
      exit_death = exit_by_death
    )
}


generate_df_main <- function(input_df, var_y, cutoff = 1, lag_i = 1) {

  var_y <- enquo(var_y)
  
  plot_based_df <- input_df |> 
    arrange(county_id, year) |> 
    mutate(
      ln_total = log(!!var_y),
      lag_ln_total = dplyr::lag(log(!!var_y), n = lag_i),
      ln_change_rate_total = log(!!var_y) - dplyr::lag(log(!!var_y), n = lag_i),
      # ln_total = log(net_internal),
      # lag_ln_total = dplyr::lag(log(net_internal)),
      # ln_change_rate_total = log(net_internal) - dplyr::lag(log(net_internal)),
      .by = county_id
    ) |>  
    mutate(ln_change_rate_total = Winsorize(ln_change_rate_total, val = quantile(ln_change_rate_total, probs = c(0.01, 0.99), na.rm = TRUE)), .by = year)  |> 
    # dplyr::filter(!total < 100) |>
    dplyr::slice_max(prop = cutoff, order_by = total, by = "year") 
}

df_input <- df_analysis
year_n <- 2018
plot_tmap <- function(year_n, df_input, var_x, var_x_chr, title_n) {
  
  var_x <- rlang::enquo(var_x)
  
  df_based <- df_input |> 
    dplyr::filter(year == year_n)

  rank_var <- df_based |> 
    # dplyr::pull(!!var_x)
    dplyr::pull(ln_change_rate_total)
  
  quantile(rank_var)
  q_25 <- quantile(rank_var)[2]
  q_50 <- quantile(rank_var)[3]
  q_75 <- quantile(rank_var)[4]
  max  <- quantile(rank_var)[5]
  
  breaks_n <- c(0, q_25, q_50, q_75, max)

  df_st <- df_based |> 
    st_as_sf()
  
  # title_name <- paste(title_n, year_n)

    # ggplot() +
      # geom_sf(data = df_based |> st_as_sf())
  ggplot() +
  geom_sf(data = df_st, aes(fill = ln_change_rate_total)) +
  # 背景色を白にする
  theme_void() +
  theme(
    legend.position = "bottom"
  )

ggplot(df_based, aes(x = ln_change_rate_total)) + geom_histogram()
  map_output <- tm_shape(df_based) +
    tmap::tm_borders(col = 'gray', lwd = 0.1) +
    tmap::tm_fill(
      # col = !!var_x,
      col = "ln_change_rate_total",
      labels = c("0% - 25%", "25% - 50%", "50% - 75%", "75% - 100%"),
      breaks = breaks_n,
      # title = title_name, 
      palette = "PuBu",
      popup.vars = c("County:" = "county_name",
                     "Total Population:" = "total",
                     "Log Change Rate:" = "ln_change_rate_total"))

map_output

    tmap::tmap_save(tm = map_output, filename = "")
    # tmap_options(check.and.fix = TRUE)

  # save_map_year(map_output, year_n, folder_name_n, extension_n)
  
  # file_name_n <- paste0(year_n, ".html")
  # file_path <- here::here('04_analyze', 'mapping', 'figure',
  #                         folder_name_n, 'html', file_name_n)
  # 
  # tmap::tmap_save(tm = map_output,
  #                 filename = file_path)

  return(map_output)
  
}


save_map_year <- function(map_input, year_n, folder_name_n, extension_n) {
  
  file_name_n <- paste0(year_n, ".", extension_n)
  file_path <- here::here('04_analyze', 'mapping', 'figure',
                          folder_name_n, extension_n, file_name_n)
  
  tmap::tmap_save(tm = map_input,
                  filename = file_path)
  
}
