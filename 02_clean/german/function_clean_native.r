generate_ls_unused_counties <- function() {

  list_unused_counties <- c(
    6633,
    # Gemeinschaftliches deutsch-luxemburgisches Hoheitsgebiet(ドイツ・ルクセンブルク共有地区)
    7000,
    # Saarland, not available in the data
    10041, 10042, 10043, 10045, 10046, 
    # Spree-Neiße, not available in the data
    12071, 
    # population 0
    13000
  )
  
  return(list_unused_counties)
}


read_df_raw <- function(year_i) {

  df_raw <- readxl::read_xlsx(
    here::here("01_data", "raw", "german", "native", paste0("3112", year_i, "_Auszug_GV.xlsx")),
    sheet = 2
  )

}




summarise_kreis_population <- function(df) {

  df_output <- df |>
    summarise(
      population_kreis = sum(population, na.rm = TRUE),
      .by = county_id
    ) |>
    rename(population = population_kreis)

}

select_filter_vars <- function(year_i) {

  df_raw <- read_df_raw(year_i)

  df <- df_raw |>
    select(
      check = 2,
      land_code = 3,
      rb_code = 4,
      kreis_code = 5,
      county_name = 8,
      population = 10
    ) |>
    mutate(
      county_id = paste0(land_code, rb_code, kreis_code)
    ) |> 
    dplyr::filter(
      !stringr::str_detect(county_id, '[^0-9.-]'),
      !is.na(check)
    ) |> 
    mutate(
      county_id = as.numeric(county_id),
      population = as.numeric(population)
    ) |>
    filter(
      !county_id %in% generate_ls_unused_counties()
    )

  
}


add_variables <- function(df_cleaned) {

  df_output <- df_cleaned |>
  # df_cleaned |> 
    mutate(
      ln_population = log(population),
      ln_change_rate = ln_population - lag(ln_population),
      .by = county_id
    )
}

clean_native_population <- function(year_i) {

  df <- select_filter_vars(year_i)

  df_cleaned <- summarise_kreis_population(df) |>
    mutate(year = year_i)
} 


aggregate_german_native_data <- function() {

  df_output <- purrr::map(2013:2022, clean_native_population) |> 
    bind_rows()

  df_master  <- add_variables(df_output)

}

select_columns <- function(df) {

  df_output <- df_raw |>
    select(
      county_id = 1,
      county_name = 2,
      "2013" = 3,
      "2014" = 5,
      "2015" = 7,
      "2016" = 9,
      "2017" = 11,
      "2018" = 13,
      "2019" = 15,
      "2020" = 17,
      "2021" = 19,
      "2022" = 21,
      "2023" = 23) |>
    dplyr::filter(!is.na(county_name)) |> 
    pivot_longer(
      cols = -c(county_id, county_name),
      names_to = "year",
      values_to = "population"
    ) |> 
    mutate(across(-c(county_name), as.numeric)) 
}


merge_counties <- function(df) {

  df_output <- df |>
    dplyr::filter(!is.na(population)) |>
    mutate(
      county_id = case_when(
        county_id %in% c(3152, 3156) ~ 3159,
        county_id %in% c(16056, 16063) ~ 16063,
        .default = county_id
      )
    ) |> 
    dplyr::filter(!str_detect(county_name, "until"))
}

