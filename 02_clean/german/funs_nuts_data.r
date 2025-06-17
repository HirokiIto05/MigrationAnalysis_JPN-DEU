clean_nuts_data <- function(df_nuts) {

  df <- df_nuts |>
    janitor::clean_names() |> 
    dplyr::filter(
      country_code == "DE",
      nuts_level %in% c(2,3)
    ) |>
    select(
      nuts_code,
      city_name = nuts_label,
      nuts_level
    ) |>
    mutate(
      nuts_code2 = if_else(
        nuts_level == 2,
        nuts_code,
        NA_character_
      ),
      # city_name = str_replace_all(city_name, "Kreisfreie Stadt", ""),
      # city_name = str_replace_all(city_name, "Landkreis", ""),
      # city_name = str_replace_all(city_name, "\\,", ""),
      city_name = str_replace_all(city_name, " ", "")
    ) |>
    fill(nuts_code2, .direction = "down")
    
}



