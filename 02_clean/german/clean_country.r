main <- function() {

  source(here("03_analysis", "analysis_funs.r"))

  df_country_raw <- purrr::map_dfr(
    c(2010, 2015, 2020),
    read_raw_data
  )

  df_country <- df_country_raw |>
    merge_municipalities() |>
    remove_unavailable_counties() |>
    summarise_total() |>
    change_country_name()

  df_country_class <- readxl::read_xlsx(
    here("01_data", "intermediate", "german", "country_classification_master.xlsx")
  )

  df_country_master <- df_country |>
    left_join(df_country_class, by = "country_name") |>
    dplyr::filter(!is.na(cls_income))

  openxlsx::write.xlsx(
    df_country_master, 
    here("01_data", "intermediate", "german", "country_class_master.xlsx")
  )
}


read_raw_data <- function(year_i) {

  df_raw <- read_csv(here("01_data", "raw", "german", "foreign", paste0("country_", year_i, ".csv")))

  df_output <- df_raw |>
    adjust_cols()

  return(df_output)
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
    dplyr::filter(country_name != "Total") |>
    mutate(
      if_else(
        country_name %in% c("Unknown / Not specified", "Stateless"), 
        "unknown",
        country_name
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


subtract_seeking_protection <- function(df) {

  df_de_asylum <- readxl::read_xlsx(here("01_data", "intermediate", "german", "protect_country.xlsx")) |>
    select(
      year, county_id, country_name, total_without_protection = total
    )

  df_output <- df |>
    left_join(df_de_asylum, by = c("year", "county_id", "country_name")) |>
    as_tibble()

  df_output |> select(total_without_protection)
    mutate(
      total_without_protection = population - if_else(is.na(), 0, total)
    )

    
  
}
