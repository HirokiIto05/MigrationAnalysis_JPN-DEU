main <- function() {
  source(here("03_analysis", "analysis_funs.r"))
  
  # Load data
  df_raw <- readxl::read_xlsx(here("01_data", "raw", "german", "marital_status", "2000X-3038_en.xlsx"))
df_raw |> View()
  df <- df_raw |>
    select(
      county_id = 1,
      year = 1,
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
