main <- function() {
  
  # 日本語の列名を英語に変換
  list_colname <- create_list_colname()

  year_list <- seq(2013, 2023)
  
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
    dplyr::mutate(city_id = stringr::str_sub(city_id, start = 1, end = -2)) |>
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
    dplyr::mutate(city_id = stringr::str_sub(city_id, start = 1, end = -2)) |>
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
    dplyr::mutate(city_id = stringr::str_sub(city_id, start = 1, end = -2)) |>
    tidyr::drop_na(city_id) |> 
    dplyr::select(-c(change_rate, natural_rate, social_rate)) 
    
  # 市町村合併データ ----------------------------------------------------------------
  adjust_df <- readxl::read_xlsx(here::here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))
  
  df_city_id_2020 <- adjust_df |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::rename(
      city_id = id_muni2020
    )

  list_2020_id <- df_city_id_2020 |> 
    dplyr::pull(city_id)

  # 市町村名とidのデータフレーム ---------------------------------------------------------
  df_name_id <- df_for_raw |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name,
      year
    ) |> 
    dplyr::filter(year == 2020) |> 
    dplyr::filter(city_id %in% list_2020_id) |>
    distinct() |>
    dplyr::select(-year)
  
  # 市町村合併の調整  
  df_jap_adj <- purrr::map(list_2020_id, adjust_city_id, df_jap_raw, adjust_df) |> 
    dplyr::bind_rows() |>
    left_join(df_name_id)

  df_for_adj <- purrr::map(list_2020_id, adjust_city_id, df_for_raw, adjust_df) |> 
    dplyr::bind_rows() |>
    left_join(df_name_id)

  df_both_adj <- purrr::map(list_2020_id, adjust_city_id, df_both_raw, adjust_df) |> 
    dplyr::bind_rows() |>
    left_join(df_name_id)

  # add lag variables
  df_jap_add <- add_lag_variables(df_jap_adj)
  df_for_add <- add_lag_variables(df_for_adj)
  df_both_add <- add_lag_variables(df_both_adj)

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


# Functions -----------------------------------------------------------------

# generate a colname's list 
create_list_colname <- function() {
  list_colname <- c(
    "city_id",
    "prefecture_name",
    "city_name",
    "male",
    "female",
    "total",
    "household",
    "moving_in_dom", # 国内からの転入者
    "moving_in_int", # 国外からの転入者
    "moving_in_total", # 転入者合計
    "birth",
    "moving_in_others", # その他合計
    "increase_total",
    "moving_out_dom", # 国内への転出者
    "moving_out_int", # 国外への転出者
    "moving_out_total", # 転出者合計
    "mortality",
    "moving_out_others", # その他合計
    "decrease_total",
    "change", # 増減数
    "change_rate",
    "natural", # 自然増減数
    "natural_rate",
    "social", # 社会増減数
    "social_rate"
  )
  
  return(list_colname)
  
}


# aggregate for changing raw data to panel data
aggregate_pop <- function(year_n, list_colname, nationality) {
  print(year_n)

  if (nationality == "japanese") {
    raw_name <- paste0((year_n -2000), "07nsjin")

    if (year_n <= 2020) {

      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, ".xls"))
      
      new_df <- readxl::read_xls(file_name, col_names = FALSE) |>  
        dplyr::select(-c(7, 8, 14, 15,
                         22, 23)) 
    } else if (year_n >= 2021) {

      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, ".xlsx"))

      new_df <- readxl::read_xlsx(file_name) |>  
        dplyr::select(-c(7, 8, 14, 15, 22, 23)) 
      
    }
    colnames(new_df) = list_colname
  } else if (nationality == "overseas") {
    raw_name <- paste0((year_n -2000), "11gsjin")

    if (year_n <= 2013) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xls'))

      new_df <-  readxl::read_xls(file_name, col_names = FALSE) |>
        dplyr::select(-c(12, 13, 14, 21, 22)) 

    } else if (year_n <= 2020) {

      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xls'))
      
      new_df <- readxl::read_xls(file_name, col_names = FALSE) |> 
        dplyr::select(-c(12, 13, 20, 21))
    } else if (year_n >= 2021) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))
      
      new_df <- readxl::read_xlsx(file_name, col_names = FALSE) |> 
        dplyr::select(-c(12, 13, 20, 21))
      
    }
    colnames(new_df) = list_colname
  } else if(nationality == "both") {

    raw_name <- paste0((year_n -2000), "03ssjin")

    if (year_n <= 2020) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xls'))
      
      new_df <- readxl::read_xls(file_name, col_names = FALSE)
    } else {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))
      
      new_df <- readxl::read_xlsx(file_name, col_names = FALSE)
    }
    }

  colnames(new_df) <- list_colname

  output_df <- new_df |>
    dplyr::mutate(year = year_n, .after = city_name) |> 
    dplyr::slice(c(-1:-4)) |> 
    dplyr::ungroup()
  
  return(output_df)
  
}


adjust_city_id <- function(id_n, df_pop, adjust_df){
  print(id_n)
  
  list_id <- adjust_df |> 
    dplyr::filter(id_muni2020 == id_n) |> 
    dplyr::select(seq(2,10)) |> 
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "year",
      values_to = "id"
    ) |> 
    dplyr::distinct(id) |> 
    dplyr::pull()
  
  df_id_n <- df_pop |> 
    dplyr::filter(city_id %in% list_id) |> 
    dplyr::mutate(
      year = as.character(year),
      city_id = as.character(city_id)
      )
  
  city_data <- df_pop |>
    dplyr::filter(year == 2020,
                  city_id == id_n) |> 
    dplyr::select(city_id, city_name, prefecture_name)
  
  city_id_n <- city_data |> 
    dplyr::pull(city_id)
  
  list_vars <- c(
    "male",
    "female",
    "total",
    "household",
    "moving_in_dom",
    "moving_in_int",
    "moving_in_total",
    "birth",
    "moving_in_others",
    "increase_total",
    "moving_out_dom",
    "moving_out_int",
    "moving_out_total",
    "mortality",
    "moving_out_others",
    "decrease_total",
    "change",
    "natural",
    "social"
  )


  output_data <- df_id_n |>
    dplyr::reframe(
      dplyr::across(dplyr::any_of(list_vars), ~sum(., na.rm = TRUE)),
      .by = year
    ) |>
    dplyr::mutate(
      city_id = city_id_n
      )
  
  return(output_data)
  
}


add_lag_variables <- function(df_input) {
  
  df_based <- df_input |>
    dplyr::group_by(city_id) |>
    dplyr::mutate(
      lag_total = dplyr::lag(total),
      change_rate = change/dplyr::lag(total)*100,
      natural_rate = natural/dplyr::lag(total)*100,
      social_rate = (social/dplyr::lag(total))*100,
      ln_total = log(total),
      ln10_total = log10(total),
      lag_ln_total = dplyr::lag(ln_total),
      lag_ln_total_5 = dplyr::lag(ln_total, n = 5),
      ln_change_rate_total = ln_total - lag_ln_total,
      ln_change_rate_total_5 = ln_total - lag_ln_total_5
    ) |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name,
      year,
      male,
      female,
      total,
      household,
      moving_in_dom,
      moving_in_int,
      moving_in_total,
      birth,
      moving_in_others,
      increase_total,
      moving_out_dom,
      moving_out_int,
      moving_out_total,
      mortality,
      moving_out_others,
      decrease_total,
      change,
      change_rate,
      natural,
      natural_rate,
      social,
      social_rate,
      lag_total,
      ln_total,
      ln10_total,
      lag_ln_total,
      lag_ln_total_5,
      ln_change_rate_total,
      ln_change_rate_total_5,
    ) |>
    ungroup()
  
  return(df_based)
  
}


change_year <- function(df) {

  df_output <- df |>
    dplyr::mutate(
      year = as.numeric(year),
      year = year - 1
    )
  
  return(df_output)
}

main()
