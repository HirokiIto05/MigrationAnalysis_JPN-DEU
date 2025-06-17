main <- function() {

  # Load data
  df_raw <- readxl::read_xlsx(here("01_data", "raw", "german", "foreign", "age_citizenship.xlsx"))

  df <- clean_colnames(df_raw) |>
    change_long_data() |>
    merge_municipalities() 

  # Save data
  write.csv(
    df,
    here("01_data", "intermediate", "german", "age_muni_master.csv"),
    row.names = FALSE
  )
}


clean_colnames <- function(df_raw) {
  
  list_colnames <- df_raw |>
    slice(3) |>
    c()

  colnames(df_raw) <- list_colnames

  df_output <- df_raw |> 
    janitor::clean_names() |>
    select(citizenship = 1, age = 2, everything()) |>
    select(-starts_with("na"))
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


change_long_data <- function(df) {

  df_output <- df |>
    clean_colnames() |>
    fill(citizenship, .direction = "down") |> 
    mutate(
      across(-c(citizenship, age), as.numeric)
    ) |> 
    dplyr::filter(!is.na(persons)) |> 
    select(-persons) |>
    dplyr::filter(citizenship == "Abroad") |>
    select(-citizenship) |>
    pivot_longer(
      cols = -age,
      names_to = "county",
      values_to = "total"
    ) |> 
    separate(
      col = county,
      into=c('county_id', 'county_name'), 
      sep='_'
    ) |> 
    mutate(
      county_id = str_remove(county_id, "x"),
      county_id = as.numeric(county_id)
    ) |>
    select(-county_name) |>
    pivot_wider(
      names_from = age,
      values_from = total
    ) |> 
    janitor::clean_names()
}


merge_municipalities <- function(df) {

  df_output <- df |>
    remove_unavailable_counties() |>
    mutate(
        county_id = case_when(
        # Mecklenburg-Vorpommern
        # Rostock and Schwerin don't change
        county_id %in% c(13055,13056,13052,13002) ~ 13071,
        county_id %in% c(13053,13051) ~ 13072,
        county_id %in% c(13057,13005,13061) ~ 13073,
        # county_id %in% c(13057,13005,13061) ~ 13073,
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
    dplyr::filter(
      # Cottbus and Saarland are not available in the dataset
      !county_id %in% c(10044, 12052)
    ) |>
    summarise(
      total = sum(total, na.rm = TRUE),
      under_10_years = sum(under_10_years, na.rm = TRUE),
      x10_to_19_years = sum(x10_to_19_years, na.rm = TRUE),
      x20_to_29_years = sum(x20_to_29_years, na.rm = TRUE),
      x30_to_39_years = sum(x30_to_39_years, na.rm = TRUE),
      x40_to_49_years = sum(x40_to_49_years, na.rm = TRUE),
      x50_bis_59_years = sum(x50_bis_59_years, na.rm = TRUE),
      x60_to_69_years = sum(x60_to_69_years, na.rm = TRUE),
      x70_to_79_years = sum(x70_to_79_years, na.rm = TRUE),
      x80_years_and_over = sum(x80_years_and_over, na.rm = TRUE),
      .by = c("county_id")
    )

}
