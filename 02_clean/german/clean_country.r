main <- function() {

  df_raw <- read.csv(here("01_data", "raw", "german", "foreign", "country.csv"))

  df <- adjust_cols(df_raw) |>
    merge_counties() |>
    remove_unavailable_counties() |>
    summarise_total()

  openxlsx::write.xlsx(
    df, 
    here("01_data", "intermediate", "german", "country_master.xlsx")
  )
}


adjust_cols <- function(df_raw) {

  df_output <- df_raw |>
    select(
      date = 1,
      county_id = 2,
      county_name = 3,
      country_name = 4,
      population = 7
    ) |>
    # fill(date, county_id, county_name, .direction = "down") |>
    mutate(
      county_id = as.numeric(county_id),
      date = lubridate::ymd(date),
      year = lubridate::year(date),
      population = str_replace_all(population, "-", "0"),
      population = as.numeric(population)
    ) |> 
    select(-date) |>
    relocate(year, .before = 1) |>
    dplyr::filter(!is.na(population)) |>
    dplyr::filter(country_name != "Total")
}


merge_counties <- function(df) {

  df_output <- df |>
    mutate(
      county_id = case_when(
        # Mecklenburg-Vorpommern
        # Rostock and Schwerin don't change
        county_id %in% c(13055,13056,13052,13002) ~ 13071,
        county_id %in% c(13053,13051) ~ 13072,
        county_id %in% c(13057,13005,13061) ~ 13073,
        county_id %in% c(13057,13005,13061) ~ 13073,
        county_id %in% c(13058,13006) ~ 13074,
        county_id %in% c(13001,13059,13062) ~ 13075,
        county_id %in% c(13054,13060) ~ 13076,
        # Gettingen
        county_id %in% c(3152, 3156) ~ 3159,
        # Eisenach
        county_id %in% c(16056, 16063) ~ 16063,
        .default = county_id
        )
    )
}


remove_unavailable_counties <- function(df) {

  # Not available on Foreigner's data
  # Description below
  df <- df |>
  dplyr::filter(
    !county_id %in% c(
      # Saarland
      10041, # Regionalverband Saarbrücken, Landkreis
      10042, # Merzig-Wadern, Landkreis
      10043, # Neunkirchen, Landkreis
      10044, # Saarlouis, Landkreis
      10045, # Saarpfalz-Kreis
      10046  # Sankt Wendel, Landkreis
    ),
    !county_id %in% c(
      12052, # Cottbus, kreisfreie Stadt
      12071  # Spree-Neiße, Landkreis
    ),
    !county_id %in% c(
      06633 # Kassel, Landkreis
    )
  )
}


summarise_total <- function(df) {

  df_output <- df |>
    summarise(
      population = sum(population, na.rm = TRUE),
      .by = c("year", "county_id", "country_name")
    )

}
