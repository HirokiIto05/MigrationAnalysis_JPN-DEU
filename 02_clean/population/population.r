main <- function() {

  source(here::here("02_clean", "population", "funs_population.r"))
  
  # 日本語の列名を英語に変換
  list_colname <- create_list_colname()

  year_list <- seq(2013, 2024)
  
  non_numeric_variables <- c("prefecture_name", "city_name")

  # clean data ---------------------------------------------------------------------
  # Japanese
  df_jap_raw <- purrr::map(year_list,
                       aggregate_pop,
                       list_colname = list_colname,
                       nationality = "japanese") |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = "japanese",
                  .after = city_name) |>
    dplyr::mutate(
      city_id = stringr::str_sub(city_id, start = 1, end = -2),
      city_id = as.numeric(city_id)
      ) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate))
  
  # Foreigners
  df_for_raw <- purrr::map(year_list,
                       aggregate_pop,
                       list_colname = list_colname,
                       nationality = "overseas") |>
    dplyr::bind_rows() |> 
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = "overseas",
                  .after = city_name) |>
    dplyr::mutate(
      city_id = stringr::str_sub(city_id, start = 1, end = -2),
      city_id = as.numeric(city_id)
      ) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate)) 

  # both
  df_both_raw <- purrr::map(
    year_list,
    aggregate_pop,
    list_colname = list_colname,
    nationality = "both"
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = "both",
                  .after = city_name) |>
    dplyr::mutate(
      city_id = stringr::str_sub(city_id, start = 1, end = -2),
      city_id = as.numeric(city_id)
      ) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate)) 

  df_jp_adjusted <- df_jap_raw |>
    adjust_munis() |>
    sumamrise_new_munis() |>
    extract_muni_2020()

  df_for_adjusted <- df_for_raw |>
    adjust_munis() |>
    sumamrise_new_munis() |>
    extract_muni_2020()

  df_both_adjusted <- df_both_raw |>
    adjust_munis() |>
    sumamrise_new_munis() |>
    extract_muni_2020()

  # add lag variables
  df_jap_add <- add_lag_variables(df_jp_adjusted) |>
    add_city_pref_name()
  df_for_add <- add_lag_variables(df_for_adjusted) |>
    add_city_pref_name()
  df_both_add <- add_lag_variables(df_both_adjusted) |>
    add_city_pref_name()



  # change year by 1
  # 日本のデータは1月1日時点を基準にしているが、ドイツは12月31日時点の人口。
  # 12月31日時点に合わせるために、日本のデータを1年ずらす。
  df_jap_master <- change_year(df_jap_add) |>
    dplyr::filter(year >= 2013)
  df_for_master <- change_year(df_for_add) |>
    dplyr::filter(year >= 2013)
  df_both_master <- change_year(df_both_add) |>
    dplyr::filter(year >= 2013)

  # save -----------------------------------------------------------------
  write.csv(df_jap_master, here::here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_for_master, here::here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932", row.names = FALSE)
  write.csv(df_both_master, here::here("01_data", "intermediate", "population", "both_master.csv"), fileEncoding = "cp932", row.names = FALSE)

}

read_population_data <- function(nationality_i) {

    purrr::map_dfr(year_list,
                       aggregate_pop,
                       list_colname = list_colname,
                       nationality = nationality_i)
}

change_type_cols <- function(df, nationality_i) {

  df |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |>
    dplyr::mutate(nationality = nationality_i,
                  .after = city_name) |>
    dplyr::mutate(city_id = stringr::str_sub(city_id, start = 1, end = -2)) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate)) 

}


