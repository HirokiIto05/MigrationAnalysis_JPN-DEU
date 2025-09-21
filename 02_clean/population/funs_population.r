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
    } else if (year_n <= 2023) {

      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, ".xlsx"))

      new_df <- readxl::read_xlsx(file_name) |>  
        dplyr::select(-c(7, 8, 14, 15, 22, 23)) 
      
    } else if(year_n >= 2024) {
      raw_name <- paste0((year_n -2000), "nsjin")
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))

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
    } else if (year_n <= 2023) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))
      
      new_df <- readxl::read_xlsx(file_name, col_names = FALSE) |> 
        dplyr::select(-c(12, 13, 20, 21))
      
    } else if(year_n >= 2024) {
      raw_name <- paste0((year_n -2000), "gsjin")
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
    } else if(year_n <= 2023) {
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))
      
      new_df <- readxl::read_xlsx(file_name, col_names = FALSE)
    } else if(year_n >= 2024) {
      raw_name <- paste0((year_n -2000), "ssjin")
      file_name <- here::here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        nationality,
        paste0(raw_name, '.xlsx'))

      new_df <- readxl::read_xlsx(file_name, col_names = FALSE)
    }
  colnames(new_df) <- list_colname
  }
  output_df <- new_df |>
    dplyr::mutate(year = year_n, .after = city_name) |> 
    dplyr::slice(c(-1:-4)) |> 
    dplyr::ungroup()
  
  return(output_df)
  
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
      # city_name,
      # prefecture_name,
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


generate_muni_mapping_table <- function() {
  # ファイル読み込み（シート名が "Sheet1" の場合）
  df_mapping_raw <- read_excel(here("01_data/raw/municipality_converter/municipality_converter_jp.xlsx"))

  # 必要な列の選択（1980年〜2020年のID列と2020年ID）
  id_cols <- grep("^id_muni", names(df_mapping_raw), value = TRUE)
  id_cols <- setdiff(id_cols, "id_muni2020") # "id_muni2020"を重複しないよう除外
  df_ids <- df_mapping_raw[, c("id_muni2020", id_cols)]

  # ロング形式に変換
  df_long <- df_ids %>%
    pivot_longer(
      cols = all_of(id_cols),
      names_to = "year",
      values_to = "other_id"
    )

  # 2020年IDと他の年のIDをマッピング、重複・NA除去、異なるIDのみ
  df_mapping <- df_long %>%
    dplyr::filter(!is.na(other_id)) %>%
    dplyr::filter(id_muni2020 != other_id) %>%
    distinct(id_muni2020, other_id) %>%
    arrange(id_muni2020, other_id)
  
  return(df_mapping)

}


adjust_munis <- function(df) {

  df_mapping <- generate_muni_mapping_table()

  df |>
    mutate(city_id = as.numeric(city_id)) |>
    left_join(df_mapping, by = c("city_id" = "other_id")) |>
    mutate(id_muni2020 = if_else(is.na(id_muni2020), city_id, id_muni2020))
}


sumamrise_new_munis <- function(df) {

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

  df |>
    dplyr::reframe(
      dplyr::across(dplyr::any_of(list_vars), ~sum(., na.rm = TRUE)),
      .by = c(year, id_muni2020)
    ) |>
    rename(city_id = id_muni2020)
}


extract_muni_2020 <- function(df) {

  df_mapping_raw <- read_excel(here("01_data/raw/municipality_converter/municipality_converter_jp.xlsx"))

  # Get all muni id existing in 2020
  all_muni2020 <- unique(df_mapping_raw$id_muni2020)
  
  # Generate all combinations of years and municipality IDs
  all_years <- df$year |> unique()
  full_index <- expand.grid(city_id = all_muni2020, year = all_years)

  # 欠損値を含めて結合（存在しない組はNA）
  df_output <- full_index |>
    left_join(df, by = c("city_id", "year")) |> 
    arrange(city_id, year)

  return(df_output)

}


generate_2020_names <- function() {

  raw_name <- paste0((2020 -2000), "07nsjin")

  df_2020 <- readxl::read_xls(here(
        "01_data",
        'raw',
        'jumin_kihon_daicho',
        "japanese",
        paste0(raw_name, '.xls')),
        col_names = FALSE) |>  
      select(
        city_id = 1,
        city_name = 3,
        prefecture_name = 2) |>
    mutate(
      city_id = stringr::str_sub(city_id, start = 1, end = -2),
      city_id = as.numeric(city_id)
      ) |>
    tidyr::drop_na(city_id) 

  return(df_2020)
}


add_city_pref_name <- function(df) {

  df_2020 <- generate_2020_names()

  df |>
    left_join(df_2020, by = "city_id") |> 
    select(
      city_id, city_name, prefecture_name,
      everything()
    ) 


}
  