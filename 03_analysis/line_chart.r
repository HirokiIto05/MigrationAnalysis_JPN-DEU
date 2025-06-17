main <- function() {

  df <- read_csv(here("01_data", "intermediate", "population", "overseas_master.csv"), locale = locale(encoding = "CP932")) |>
    select_cols_jp()

  df_native <- read_csv(here("01_data", "intermediate", "population", "japanese_master.csv"), locale = locale(encoding = "CP932")) |>
    select_cols_jp() |>
    select(total_native = total, year, county_id)

  # Plot
  df_plot <- generate_df_plot(df, df_native, list_var_order = NULL)
  plot_line_chart(df_plot, country)

  # Plot by Region
  ## By country
  df_pref_region <- read_xlsx(here("01_data", "intermediate", "foreign_status", "prefecture_by_region.xlsx"))

  df_region <- df_pref_region |>
    summarise(
      total = sum(population, na.rm = TRUE),
      .by = c(year, region)
    ) |>
    add_growth_rate("total", list_var_order = c("region", "year")) |>
    rename(value = growth_rate) 

  plot_line_chart(df_region, region)

  # By Urban and Rural
  list_urban_pref <- c("東京", "神奈川", "埼玉", "千葉", "大阪", "愛知")

  df_pref_urban <- df_pref_region |> 
    mutate(is_urban = ifelse(prefecture %in% list_urban_pref, "Urban", "Rural")) |>
    summarise(
      total = sum(population, na.rm = TRUE),
      .by = c(year, is_urban, region)
    ) |>
    add_growth_rate("total", list_var_order = c("is_urban", "region", "year")) |> 
    rename(value = growth_rate) 
      
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

add_growth_rate <- function(df_i, column_i, list_var_order) {

  df_output <- df_i |>
    arrange(!!! rlang::syms(list_var_order)) |>
    # arrange(is_urban,region, year) |> View()
    mutate(
      base_13 = ifelse(year == 2013, !!sym(column_i), NA)
      ) |> 
    fill(base_13, .direction = "down") |>
    mutate(
      growth_rate = round((!!sym(column_i) / base_13)*100, digits = 3)
    ) |>
    select(-base_13)

}


generate_df_plot <- function(df, df_native, list_var_order) {

  df_sum <- df |>
    summarise(
      total = sum(total, na.rm = TRUE),
      .by = year
    ) |>  
    # add_growth_rate("total", list_var_order) 
    add_growth_rate("total", c("country")) 

  df_sum_native <- df_native |>
    summarise(
      total = sum(total_native, na.rm = TRUE),
      .by = year
    ) |>
    add_growth_rate("total", list_var_order) |>
    rename(
      growth_rate_native = growth_rate,
      total_native = total) 


  df_plot <- df_sum |>
    left_join(df_sum_native) |> 
    select(-starts_with("total")) |> 
    pivot_longer(
      cols = -c(year),
      names_to = "country",
      values_to = "value"
    )


}


plot_line_chart <- function(df_plot, var_color) {

  var_color <- enquo(var_color)

  plot_output <- df_plot |>
    ggplot(aes(x = year, y = value, color = !!var_color)) + 
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_x_continuous(
      breaks = seq(2013, 2019, 1),
      limits = c(2013, 2019)
      )

  return(plot_output)
}


plot_line_chart_urban <- function(df_pref_urban, var_color) {

  var_color <- enquo(var_color)

  plot_output <- df_pref_urban |>
    ggplot(aes(x = year, y = value, color = region)) + 
    # ggplot(aes(x = year, y = value, color = !!var_color)) + 
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_x_continuous(
      breaks = seq(2013, 2019, 1),
      limits = c(2013, 2019)
      ) +
    facet_wrap(~is_urban)

  return(plot_output)
}





