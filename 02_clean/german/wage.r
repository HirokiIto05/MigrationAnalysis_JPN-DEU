
main <- function() {

  source(here("03_analysis", "analysis_funs.r"))

  df_raw <- readxl::read_excel(here("01_data", "raw", "german", "wage", "vgrdl_r2b2_bs2023_0.xlsx"), sheet = "wage")

  df <- df_raw |>
    change_colname() |>
    adjust_wage()

  write.csv(df, here("01_data", "intermediate", "german", "wage.csv"), row.names = FALSE)

}


change_colname <- function(df_raw) {

  list_colnames <- df_raw |>
    slice(4) |>
    c()

  colnames(df_raw) <- list_colnames

  df_output <- df_raw |> 
    janitor::clean_names() |> 
    select(
      county_id = 3,
      starts_with("x")) |> 
    mutate(
      across(everything(), as.numeric),
      county_id = if_else(county_id == 11, 11000, county_id),
      county_id = if_else(county_id == 2, 2000, county_id),
    ) |>
    dplyr::filter(!county_id < 1000) |>
    remove_unavailable_counties()

}


adjust_wage <- function(df) {

  df_output <- df |> 
    pivot_longer(
      cols = -county_id,
      names_to = "year") |>
    mutate(
      year = str_remove(year, "x"),
      year = as.numeric(year)
    )
}
