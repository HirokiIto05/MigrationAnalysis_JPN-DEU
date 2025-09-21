# DESTATIS 
# Home ->Statistics -> Statistics of persons seeking protection
# Statistics -> 
# Statistics of persons seeking protection -> 
# Persons seeking protection: Administrative districts, reference date, sex, country groups/citizenship

main <- function() {

  df_protectjj_raw <- readxl::read_xlsx(
    here("01_data", "raw", "german", "foreign", "protection.xlsx")
  )

  # by country
  df_protect_raw_country1 <- read_csv(
    here(
      "01_data",
      "raw",
      "german",
      "foreign",
      "protection_country_10_17.csv")) 

  df_protect_raw_country2 <- read_csv(
    here(
      "01_data",
      "raw",
      "german",
      "foreign",
      "protection_country_18_23.csv")) 

  
  df_protect_output <- change_colnames_protect(df_protect_raw)
  df_protect_output_country <- change_colnames_protect_country(df_protect_raw_country) |>
    change_country_name() 

  # write data
  openxlsx::write.xlsx(
    df_protect_output,
    here("01_data", "intermediate", "german", "protect.xlsx"))
  # by country
  openxlsx::write.xlsx(
    df_protect_output_country,
    here("01_data", "intermediate", "german", "protect_country.xlsx"))
}


change_colnames_protect_country <- function(df) {

  df_output <- df |>
    select(
      year = 1,
      county_id = 2,
      county_name = 3,
      country_name = 4,
      total = 7
    ) |>
  dplyr::filter(!is.na(county_id)) |> 
  mutate(
    year = lubridate::year(year),
    total = str_replace_all(total, "-", "0"),
    across(-c(county_name, country_name), as.numeric)
  ) |>
  dplyr::filter(country_name != "Total")

}


change_colnames_protect <- function(df) {

  # df_output <- df |>
  df_protect_raw |>
    select(
      year = 1,
      county_id = 2,
      county_name = 3,
      total = 8
    ) |>
  dplyr::filter(!is.na(county_id)) |> 
  fill(year, .direction = "down") |>
  mutate(
    year = lubridate::year(year),
    total = str_replace_all(total, "-", "0"),
    across(-c(county_name), as.numeric)
  ) 

}





# 