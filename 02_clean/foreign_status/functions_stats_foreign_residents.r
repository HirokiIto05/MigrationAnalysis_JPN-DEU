# This file contains functions to clean Statistics on foreign residents
# https://www.moj.go.jp/isa/policies/statistics/toukei_ichiran_touroku.html?hl=ja

change_colnames <- function(df) {
  df <- df |>
    clean_names() |> 
    select(
      region = 1,
      country = 2,
      population = 3
      )

  return(df)
}



change_colnames_after_2019 <- function(df) {

  df <- df |>
    clean_names() |>
    select(
      country = 1,
      population = 2
      ) |>
    mutate(
      region = if_else(country %in% c("アジア", "アフリカ", "ヨーロッパ", "北米", "北アメリカ", "南米", "南アメリカ", "オセアニア"), country, NA),
      .before = country
    )

  return(df)
}


change_colnames_after_2022 <- function(df) {

  df <- df |>
    clean_names() |>
    select(
      region = 2,
      country = 3,
      cat_age = 4,
      cat_gender = 5,
      population = x6
      )

  return(df)
}


#' Adjust region column
#' 
#' @description 
#' change region name into English
#' 
#' @param df: DataFrame
adjust_region_column <- function(df) {
  df <- df |>
    mutate(
      region = case_when(
        region == "アジア" ~ "Asia",
        region == "アフリカ" ~ "Africa",
        region == "ヨーロッパ" ~ "Europe",
        region == "北米" ~ "NorthAmerica",
        region == "北アメリカ" ~ "NorthAmerica",
        region == "南米" ~ "SouthAmerica",
        region == "南アメリカ" ~ "SouthAmerica",
        region == "オセアニア" ~ "Oceania",
        region == "無国籍" ~ "Unknown",
        TRUE ~ NA
      )
    )
    
}


filter_age_and_gender <- function(df) {
  
  df <- df |> filter(
    cat_age == "総数",
    cat_gender == "計") |>
    select(-c(cat_age, cat_gender))

}


#' Clean foreign residents data
#' 
#' @description
#' wrapper function from reading raw data to cleaning data
clean_foreign_total_by_region <- function(year_i) {

  year_adjusted <- year_i - 2000

  if (year_i <= 2021) {
    file_name_i <- paste0(year_adjusted, "-12-02-1", ".xlsx")
  } else if (year_i == 2022) {
    file_name_i <- paste0(year_adjusted, "-12-02m", ".xlsx")
  } else if (year_i == 2023) {
    file_name_i <- paste0(year_adjusted, "-12-03m", ".xlsx")
  }

  df_raw <- read_xlsx(here::here("01_data/raw/foreign_residents_age", file_name_i))

  if(year_i <= 2021) {
    df <- change_colnames(df_raw)
    df <- adjust_region_column(df) |>
      filter(!is.na(region))
  } else if(year_i >= 2022) {
    df <- change_colnames_after_2022(df_raw)

    df <- adjust_region_column(df) |>
      filter(!is.na(region))

    df <- filter_age_and_gender(df) |>
      filter(country == "総数")
  }

  df_output <- df |>
    select(-country) |>
    mutate(year = year_i)

  return(df_output)
}

#' Clean foreign residents data
#' 
#' @description
#' wrapper function from reading raw data to cleaning data
clean_foreign_total_by_country <- function(year_i) {

  year_adjusted <- year_i - 2000

  if (year_i <= 2021) {
    file_name_i <- paste0(year_adjusted, "-12-02-1", ".xlsx")
  } else if (year_i == 2022) {
    file_name_i <- paste0(year_adjusted, "-12-02m", ".xlsx")
  } else if (year_i == 2023) {
    file_name_i <- paste0(year_adjusted, "-12-03m", ".xlsx")
  }

  df_raw <- read_xlsx(here::here("01_data/raw/foreign_residents_age", file_name_i))
  if(year_i <= 2018) {
    df <- change_colnames(df_raw)
    df <- adjust_region_column(df)
    df <- change_country_name_jpn_eng(df)
  } else if(year_i <= 2021) {
    df <- change_colnames_after_2019(df_raw)
    df <- adjust_region_column(df)
    df <- change_country_name_jpn_eng(df) |>
      fill(region, .direction = "down")
  } else if(year_i >= 2022) {
    df <- change_colnames_after_2022(df_raw)
    df <- adjust_region_column(df)
    df <- filter_age_and_gender(df)
    df <- change_country_name_jpn_eng_after_2022(df)
  }

  df_output <- df |>
    # select(-country_jpn) |>
    mutate(year = year_i)

  return(df_output)
}


change_country_name_jpn_eng <- function(df) {
  
  df_name <- readxl::read_xlsx(here::here("01_data/raw/others/country_name.xlsx"))

  df <- df |>
    mutate(
      country = if_else(country == "セントクリストファー・ネービス", "セントクリストファー・ネーヴィス", country),
      country = if_else(country == "コンゴ共和国", "コンゴ民主共和国", country),
      country = if_else(country == "エスワティニ", "スワジランド", country),
      country = if_else(country == "ジョージア", "グルジア", country)
    ) |>
    left_join(df_name, by = c("country" = "country_jpn")) |> 
      mutate(
        iso_code = if_else(country == "韓国", "KOR", iso_code),
        country_eng = if_else(country == "韓国", "the Republic of Korea", country_eng),
        iso_code = if_else(country == "朝鮮", "PRK", iso_code),
        country_eng = if_else(country == "朝鮮", "the Democratic People's Republic of Korea", country_eng),
      ) |>
    select(
      region,
      country_jpn = country,
      country_eng,
      iso_code,
      population
      ) |> 
    mutate(
      country_jpn = if_else(!is.na(region), NA, country_jpn)
    )

}


change_country_name_jpn_eng_country <- function(df) {
  
  df_name <- readxl::read_xlsx(here::here("01_data/raw/others/country_name.xlsx"))

  df <- df |>
    mutate(
      country = if_else(country == "セントクリストファー・ネービス", "セントクリストファー・ネーヴィス", country),
      country = if_else(country == "コンゴ共和国", "コンゴ民主共和国", country),
      country = if_else(country == "エスワティニ", "スワジランド", country),
      country = if_else(country == "ジョージア", "グルジア", country)
    ) |>
    left_join(df_name, by = c("country" = "country_jpn")) |> 
      mutate(
        # 2015年から別れる
        iso_code = if_else(country == "韓国", "KOR", iso_code),
        country_eng = if_else(country == "韓国", "the Republic of Korea", country_eng),
        iso_code = if_else(country == "朝鮮", "PRK", iso_code),
        country_eng = if_else(country == "朝鮮", "the Democratic People's Republic of Korea", country_eng),
      ) |>
    select(
      # region,
      prefecture,
      country_jpn = country,
      country_eng,
      iso_code,
      population
      )
}


#' Clean foreign residents data in germany
#' 
#' @description 
#' This data is from the Federal Statistical Office of Germany
#' Statistics number : 12521-0002
clean_german_foreign_data <- function() {

  df_raw <- readxl::read_xlsx(here::here("01_data/raw/german/foreign_residents/12521-0002.xlsx"))

  df <- df_raw |>
    select(
      country = 1,
      male = 2,
      female = 3,
      total = 4
    ) |>
    mutate(
      year = if_else(str_detect(country, "12-31"), country, NA),
      year = if_else(str_detect(year, "Czechoslovakia"), NA, year),
      year = if_else(str_detect(year, "Until 1989-12-31"), NA, year),
      .before = country
    ) |>
    fill(year, .direction = "down") |>
    filter(
      !is.na(year),
      year != country,
      !country %in% c("Total", "of which:")
      ) |>
    filter(
      between(lubridate::year(year), 2013, 2023)
    ) |>
    mutate(
      total = as.numeric(total)
    ) |>
    select(
      country,
      year,
      total
    )
}


add_german_data_to_countrycode <- function(df_german) {

  df_country_code <- readr::read_csv(here::here("01_data/raw/others/CountryDataCodes.csv"))

  df_country_code <- df_country_code |> 
    select(
      country = 1,
      iso_code = 2
    )

  df_output <- df_german |>
    left_join(df_country_code, by = c("country" = "country")) |> 
    mutate(
      iso_code = if_else(country == "Afghanistan, Republic of", "AFG", iso_code),
      iso_code = if_else(country == "Bahamas", "BHS", iso_code), 
      iso_code = if_else(country == "Bolivia, Plurinational State", "BOL", iso_code),
      iso_code = if_else(country == "British Overseas Territories", "GBR", iso_code),
      iso_code = if_else(country == "Brunei Darussalam", "BRN", iso_code),
      iso_code = if_else(country == "Gambia", "GMB", iso_code),
      iso_code = if_else(country == "Iran, Islamic Republic of", "IRN", iso_code),
      iso_code = if_else(country == "Yugoslavia, Socialist Federal Rep. (u. 1992-04-26)", "YUG", iso_code),
      iso_code = if_else(country == "Yugoslavia, Federal Rep.(1992-04-27 to 2003-02-04)", "YUG", iso_code),
      iso_code = if_else(country == "Congo, The Democratic Republic of the", "COD", iso_code),
      iso_code = if_else(country == "Korea, Republic of", "KOR", iso_code),
      iso_code = if_else(country == "Korea, Democratic People's Republic", "PRK", iso_code),
      iso_code = if_else(country == "Lao People's Democratic Republic", "LAO", iso_code),
      iso_code = if_else(country == "Moldova, Republic of", "MDA", iso_code),
      iso_code = if_else(country == "Montenegro (since 2006-06-03)", "MNE", iso_code),
      iso_code = if_else(country == "Palestinian Territories", "PSE", iso_code),
      iso_code = if_else(country == "Russian Federation", "RUS", iso_code),
      iso_code = if_else(country == "Serbia (incl. Kosovo) (2006-06-03 to 2008-02-16)", "SRB", iso_code),
      iso_code = if_else(country == "Serbia and Montenegro (2003-02-05 to 2006-06-02)", "SRB", iso_code),
      iso_code = if_else(country == "Soviet Union (until 1991-12-25)", "RUS", iso_code),
      iso_code = if_else(country == "Sudan (including South Sudan) (until 2011-07-08)", "SDN", iso_code),
      iso_code = if_else(country == "Sudan (without South Sudan) (since 2011-07-09)", "SDN", iso_code),
      iso_code = if_else(country == "South Sudan (since 2011-07-09)", "SSD", iso_code),
      iso_code = if_else(country == "Syrian", "SYR", iso_code),
      iso_code = if_else(country == "Czechoslovakia (until 1992-12-31)", "CZE", iso_code),
      iso_code = if_else(country == "Turkey", "TUR", iso_code),
      iso_code = if_else(country == "Vatican City State", "VAT", iso_code),
      iso_code = if_else(country == "Venezuela, Bolivarian Republic of", "VEN", iso_code),
      iso_code = if_else(country == "Viet Nam", "VNM", iso_code),
      iso_code = if_else(country == "Stateless", "XXX", iso_code),
      iso_code = if_else(country == "Unknown / Not specified", "XXX", iso_code)
    ) |>
    filter(!is.na(iso_code))

}


read_prefecture_by_country <- function(year_i) {

  year_adjusted <- year_i - 2000

  file_name_i <- paste0(year_adjusted, "-12-04", ".xlsx")

  df_raw <- read_xlsx(here::here("01_data/raw/foreign_residents_by_prefecture", file_name_i), skip = 2)
}


clean_prefecture_by_region <- function(year_i) {

  print(year_i)
  df_raw <- read_prefecture_by_country(year_i)

  if (year_i <= 2018) {
    df <- df_raw |> 
      select(
        -1,
        prefecture = 2,
        where(is.numeric)
      )
  } else if (year_i >= 2019) {
    df <- df_raw |> 
      select(
        prefecture = 1,
        where(is.numeric)
        )
  } 

  df <- df |>  
    filter(!is.na(prefecture)) |> 
    pivot_longer(
      cols = -c(prefecture),
      names_to = "region",
      values_to = "population"
    ) |>  
    filter(
      region %in% 
      c("アジア",
      "アフリカ",
      "ヨーロッパ",
      "北米",
      "北アメリカ",
      "南米",
      "南アメリカ",
      "オセアニア",
      "無国籍")
    ) |>
    mutate(
      prefecture = str_replace_all(prefecture, " ", ""),
      prefecture = str_replace_all(prefecture, "県", ""),
      prefecture = str_replace_all(prefecture, "東京都", "東京"),
      prefecture = str_replace_all(prefecture, "京都府", "京都"),
      prefecture = str_replace_all(prefecture, "大阪府", "大阪"),
      year = year_i
    ) |>
    filter(!prefecture %in% c("合計", "総数"))

  df <- adjust_region_column(df)

  return(df)
}


clean_prefecture_by_country <- function(year_i) {

  print(year_i)
  df_raw <- read_prefecture_by_country(year_i)

  if (year_i <= 2018) {
    df <- df_raw |> 
      select(
        -1,
        prefecture = 2,
        where(is.numeric)
      )
  } else if (year_i >= 2019) {
    df <- df_raw |> 
      select(
        prefecture = 1,
        where(is.numeric)
        )
  } 

  df <- df |>
    filter(!is.na(prefecture)) |> 
    pivot_longer(
      cols = -c(prefecture),
      names_to = "country",
      values_to = "population"
    ) |> 
    change_country_name_jpn_eng_country() |>
    mutate(
      prefecture = str_replace_all(prefecture, " ", ""),
      prefecture = str_replace_all(prefecture, "県", ""),
      prefecture = str_replace_all(prefecture, "東京都", "東京"),
      prefecture = str_replace_all(prefecture, "京都府", "京都"),
      prefecture = str_replace_all(prefecture, "大阪府", "大阪"),
      year = year_i
    ) |>
    dplyr::filter(
      !is.na(country_eng),
      prefecture != "総数")
  return(df)
}
