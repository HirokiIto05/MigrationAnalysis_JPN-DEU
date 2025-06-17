main <- function() {

  # Total is not equal to the sum of all regions.(I don't know why)
  # But it's slightly different.
  
  df_raw <- readxl::read_xlsx(here("01_data", "raw", "german", "foreign", "area.xlsx"))

  df <- df_raw |> 
    change_colnames() |>
    fill_na() |> 
    merge_counties() |> 
    remove_unavailable_counties() |> 
    dplyr::filter(!is.na(county_id)) |>  
    filter_area() |>
    mutate(year = lubridate::year(year))

  df_merge <- summarise_merge(df)

  openxlsx::write.xlsx(
    df, 
    here("01_data", "intermediate", "german", "population_region.xlsx")
  )

}


change_colnames <- function(df_raw) {

  df <- df_raw |>
    select(
      year = 1,
      county_id = 2,
      county_name = 3,
      region = 4,
      population = 7
    )
}


fill_na <- function(df) {

  df_output <- df |>
    fill(year, .direction = "down") |>
    fill(county_id, .direction = "down") |>
    fill(county_name, .direction = "down") |>
    mutate(
      county_id = as.numeric(county_id),
      population = as.numeric(population)
    ) 

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


summarise_merge <- function(df) {

  df_output <- df |>
    summarise(
      total = sum(population, na.rm = TRUE),
      .by = c(year, region, county_id)
    ) 

}



filter_area <- function(df) {

  df_output <- df |>
    # mutate(
    #   if_else(area == "North America", "NorthAmerica", area),
    #   if_else(area == "South America", "SouthAmerica", area)) |>
    dplyr::filter(
      region %in% c(
        "Total",
        "Europe",
        "Africa",
        "North America",
        "South America",
        "Asia",
        "Australia and Oceania"
        )
    )
}

