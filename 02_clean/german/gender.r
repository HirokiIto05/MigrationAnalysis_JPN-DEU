main <- function() {
  source(here("03_analysis", "analysis_funs.r"))
  
  # Load data
  df_raw <- readxl::read_xlsx(here("01_data", "raw", "german", "gender", "12521-0040_en.xlsx"))

  df <- df_raw |>
    select(
      year = 1,
      county_id = 2,
      male = 4,
      female = 6,
      total = 8
    )
  
  df_gender <- df |>
    fill_year() |> 
    dplyr::filter(!is.na(total)) |>
    merge_municipalities() |> 
    summarise(
      total = sum(total, na.rm = TRUE),
      male = sum(male, na.rm = TRUE),
      female = sum(female, na.rm = TRUE),
      .by = c(year, county_id)
      ) |>
    remove_unavailable_counties()
  
  
  # Save data
  write.csv(df_gender, here("01_data", "intermediate", "german", "gender.csv"), row.names = FALSE)


}


fill_year <- function(df) {

  df_output <- df |>
    fill(year, .direction = "down") |> 
    mutate(
      year = str_sub(year, start = 1, end = 4),
    ) |>
    mutate(
      across(everything(), as.numeric)
    ) |>
    dplyr::filter(!is.na(year))
    
}
