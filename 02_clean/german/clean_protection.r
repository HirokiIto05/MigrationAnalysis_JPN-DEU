# DESTATIS 
# Home ->Statistics -> Statistics of persons seeking protection
# Statistics -> 
# Statistics of persons seeking protection -> 
# Persons seeking protection: Administrative districts, reference date, sex, country groups/citizenship

# 2013 - 2019
main <- function() {
  
  df_protect_raw <- read_csv(
    here(
      "01_data",
      "raw",
      "german",
      "foreign",
      "protection.csv")) 

  df_protect_output <- change_colnames_protect(df_protect_raw)

  # write data
  openxlsx::write.xlsx(
    df_protect_output,
    here("01_data", "intermediate", "german", "protect_country.xlsx"))
}


change_colnames_protect <- function(df) {

  # df_output <- df |>
  df_output <- df_protect_raw |>
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
