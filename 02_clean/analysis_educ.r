main <- function() {

  df_educ <- readxl::read_xlsx(here("01_data", "intermediate", "german", "educ_master.xlsx"))
  df_educ <- select_cols_educ(df_educ)

  df_age <- readxl::read_xlsx(here("01_data", "intermediate", "german", "age_master.xlsx")) |> 
    select(city_name, y25_foreign) |>
    mutate(year = 2011)
  
  df_age_analysis <- merge_educ_age(df_educ, df_age)

  openxlsx::write.xlsx(
    df_age_analysis, 
    here("01_data", "analysis", "educ_analysis.xlsx")
  )
}


select_cols_educ <- function(df_educ) {

  df_output <- df_educ |>
    select(city_name, year, ends_with("foreign"))

}


merge_educ_age <- function(df_educ, df_age) {

  df_output <- df_educ |>
    left_join(df_age, by = c("city_name", "year")) |>
    dplyr::filter(year == 2011) |> 
    mutate(
      educat_1_foreign = round(educat_1_foreign * y25_foreign),
      educat_2_foreign = round(educat_2_foreign * y25_foreign),
      educat_3_foreign = round(educat_3_foreign * y25_foreign)
    )
}
