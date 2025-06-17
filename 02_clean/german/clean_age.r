main <- function() {

  df_age_raw <- readxl::read_xlsx(
    here(
    "01_data",
    "raw",
    "german",
    "foreign",
    "age.xlsx")
  )

  df_age <- clean_age(df_age_raw) |>
    remove_unavailable_counties()

  df_cor_raw <- readxl::read_xlsx(here("01_data", "intermediate", "german", "nuts_correspondence.xlsx"))

  df_cor <- df_cor_raw |>
    select_cor_data()

  df_age_master <- merge_age_cor(df_age, df_cor)

  # write data
  openxlsx::write.xlsx(
    df_age_master,
    here("01_data", "intermediate", "german", "age_master.xlsx")
  )
}


clean_age <- function(df_age_raw) {

  df_output <- df_age_raw |>
    select(
      category = 1,
      county_id = 2,
      county_name = 3,
      y25_29 = 6,
      y30_34 = 8,
      y35_39 = 10,
      y40_44 = 12,
      y45_49 = 14,
      y50_54 = 16,
      y55_59 = 18,
      y60_64 = 20
    ) |> 
    fill(category, .direction = "down") |> 
    mutate(
      across(-c(category, county_name), as.numeric)
    ) |>
    dplyr::filter(!is.na(county_id)) |> 
    pivot_longer(
      cols = -c(category, county_id, county_name),
      names_to = "age",
      values_to = "value"
    ) |>  
    summarise(
      y25_64 = sum(value, na.rm = TRUE),
      # .by = c(category, county_id)
      .by = c(category, county_id, county_name)
    ) |> 
    pivot_wider(
      names_from = category,
      values_from = y25_64
    ) |> 
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
    ) |> 
    summarise(
      y25_total = sum(Total, na.rm = TRUE),
      y25_native = sum(Germany, na.rm = TRUE),
      y25_foreign = sum(Abroad, na.rm = TRUE),
      .by = county_id
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
    # Brandenburg(NUTS2)
    !county_id %in% c(
      12052, # Cottbus, kreisfreie Stadt
      12071  # Spree-Neiße, Landkreis
    ),
    # Kassel(NUTS2)
    !county_id %in% c(
      06633 # Kassel, Landkreis
      # Kassel, Kreisfreie Stadt is available.
    )
  )


}


select_cor_data <- function(df_cor_raw) {

  df_output <- df_cor_raw |> 
    select(
      county_id,
      city_name 
    )
}


merge_age_cor <- function(df_age, df_cor) {

  # merge data
  df_age_master <- df_age |>
    left_join(df_cor, by = "county_id") |>
    summarise(
      y25_total = sum(y25_total, na.rm = TRUE),
      y25_foreign = sum(y25_foreign, na.rm = TRUE),
      y25_native = sum(y25_native, na.rm = TRUE),
      .by = city_name
    )
}
