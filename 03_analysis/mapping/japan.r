main <- function() {

  # Read and Clean data
  df_geo_raw <- sf::read_sf(here("01_data", "raw", "geodata", "japan", "prefecture.json"))
  df_geo <- adjust_geo_data(df_geo_raw)

  df <- read_csv(here("01_data", "intermediate", "population", "overseas_master.csv"), locale = locale(encoding = "CP932")) |>
    select_cols_jp()

  df_both <- read_csv(here("01_data", "intermediate", "population", "both_master.csv"), locale = locale(encoding = "CP932")) |>
    select_cols_jp() |> 
    select(total_both = total, year, county_id)

  df_native <- read_csv(here("01_data", "intermediate", "population", "japanese_master.csv"), locale = locale(encoding = "CP932")) |>
    select_cols_jp() |> 
    mutate(
      total - dplyr::lag(total),
      decrease = exit + death) |> 
    select(year, county_id, decrease) 

  df <- df |>
    left_join(df_both, by = c("county_id", "year")) 

  df_complement <- df |>
    left_join(df_native, by = c("county_id", "year")) |>
    mutate(
      complement = if_else((total - dplyr::lag(total) - ) > 0, 1, 0)
      )

  sum(df_complement$complement)

  df_pref <- summarise_prefecture(df)

  # Merge data
  df_analysis <- df_pref |>
    left_join(df_geo, by = c("prefecture_name")) |>
    add_outcomes()


  # Plot Growth rate
  plot_19_growth <- create_map(df_analysis, 2019, "growth_rate")
  plot_19_total <- create_map(df_analysis, 2019, "total_both")

  # Plot Share
  plot_13_share <- create_map(df_analysis, 2013, "share")
  plot_13_share
  plot_19_share <- create_map(df_analysis, 2019, "share")
  plot_19_share
  

}


select_cols_jp <- function(df_jp) {

  df_output <- df_jp |>
    select(
      county_id = city_id, county_name = city_name, prefecture_name, year, total,
      add = moving_in_total,
      add_internal = moving_in_dom,
      add_external = moving_in_int,
      birth,
      exit = moving_out_total,
      exit_internal = moving_out_dom,
      exit_external = moving_out_int,
      death = mortality
    )
}


adjust_geo_data <- function(df_geo_raw) {

  df_output <- df_geo_raw |>
    select(
      prefecture_name = N03_001,
      geometry
    )

}


summarise_prefecture <- function(df_jp) {

  df_output <- df_jp |>
    summarise(
      total = sum(total),
      total_both = sum(total_both),
      add = sum(add),
      add_internal = sum(add_internal),
      add_external = sum(add_external),
      add_birth = sum(birth),
      exit = sum(exit),
      exit_internal = sum(exit_internal),
      exit_external = sum(exit_external),
      exit_death = sum(death),
      .by = c(prefecture_name, year)
    )


}


add_outcomes <- function(df_analysis) {

  df_output <- df_analysis |>
    mutate(
      total_foreigner = sum(total, na.rm = TRUE),
      .by = year
      ) |>
    mutate(
      share = round((total / total_foreigner), 3)
    ) |>
    mutate(
      base_13 = ifelse(year == 2013, total, NA),
      .by = prefecture_name
      ) |> 
    fill(base_13, .direction = "down") |>
    arrange(prefecture_name, year) |>
    mutate(
      growth_rate = round((total / base_13)*100, digits = 3)
    ) 

}


create_map <- function(df_analysis, year_i, outcome_i) {

  df_input <- df_analysis |> 
    dplyr::filter(year == year_i) |>
    st_as_sf()

  mean_outcome <- df_input |>
    pull(!!sym(outcome_i)) %>%
    mean(., na.rm = TRUE)

  df_input <- df_input |>
    mutate(
      sub_mean = !!sym(outcome_i) - mean_outcome
    )
    
  plot_output <- ggplot() +
    geom_sf( 
      data = df_input,
    # aes(fill = !!sym(outcome_i)), lwd = 0)+ 
    aes(fill = sub_mean), lwd = 0)+ 
    scale_fill_gradient2(midpoint = mid) +
    # white background
    theme_void() +
    labs(title = paste0(year_i, ": ", outcome_i)) +
    # facet_wrap(~ year) +
    theme(
      legend.position = "bottom",
      title = element_text(size = 20),
    ) 
    # scale_color_gradient(low="blue", high="red")

  return(plot_output)
}

plot_13_share <- create_map(df_analysis, 2013, "share")
plot_13_share
plot_19_share <- create_map(df_analysis, 2019, "share")
plot_19_share


create_map_share <- function(df_analysis, year_i, outcome_i) {

    
  plot_output <- ggplot() +
    geom_sf( 
      data = df_analysis |> 
               dplyr::filter(year == year_i) |>
               st_as_sf(),
    aes(fill = !!sym(outcome_i)), lwd = 0)+ 
    # white background
    theme_void() +
    labs(title = paste0(year_i, ": ", outcome_i)) +
    # facet_wrap(~ year) +
    theme(
      legend.position = "bottom",
      title = element_text(size = 20),
    ) +
    scale_fill_grey()

  return(plot_output)
}