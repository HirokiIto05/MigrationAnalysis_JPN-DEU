# EUROSTAT 
# Detailed datasets -> Education and training outcomes -> 
# Educational attainment level -> Population by educational attainment level
# Population by educational attainment level, sex, age, citizenship and NUTS 2 region (%) (edat_lfs_9918)
# Age class: 25-64 years [Y25-64]

main <- function() {
  
  df_educ_raw <- readxl::read_xlsx(
    here(
      "01_data",
      "raw",
      "german",
      "eurostat",
      "educational_attainment_25.xlsx"),
      sheet = 3) 
  
  df_educ <- clean_education(df_educ_raw) |>
    complement_percent("foreign", educat_1_foreign, educat_2_foreign, educat_3_foreign)

  # Load NUTS2 and 3 correspondence data
  # df_corrs <- readxl::read_xlsx(
  #   here("01_data", "intermediate", "german", "nuts_correspondence.xlsx"))
  
  # # Population Data
  # df_german_foreign <- read.csv(here("01_data", "intermediate", "german", "foreign_master.csv")) |>
  #   dplyr::filter(!str_detect(county_name, "until")) |>
  #   dplyr::filter(
  #     !county_name %in% c(
  #       "Saarlouis, Landkreis",
  #       "Cottbus, kreisfreie Stadt"
  #     )
  #   )

  # df_pop_de <- merge_cores_data(df_german_foreign, df_corr)
  # df_pop_de <- aggregate_population_by_nuts2(df_pop_de)

  df_de_age <- read_age_de()

  df_educ_master <- change_percent_to_level(df_educ, df_de_age)

  # write data
  openxlsx::write.xlsx(
    df_educ_master,
    here("01_data", "intermediate", "german", "educ_master.xlsx"))
}


read_age_de <- function() {

  readxl::read_excel(
    here("01_data/intermediate/german/age_master.xlsx")
  ) 
  # We can use only 2011 and 2022 data, so we use 2011 data 
  # as a 2010 and 2022 data as a 2020 data.
  # mutate(
  #   year = case_when(
  #     year == 2011 ~ 2010,
  #     year == 2022 ~ 2020,
  #     .default = year
  #   )
  #   )
  
}

clean_education <- function(df_educ_raw) {

  df_educ <- df_educ_raw |>
    select(
      city_name = 1,
      year = 2,
      eu_educat_1 = 3, # Less than primary, primary and lower secondary education (levels 0-2)
      eu_educat_2 = 5, # Upper secondary and post-secondary non-tertiary education (levels 3 and 4)
      eu_educat_3 = 7, # Tertiary education (levels 5-8)
      non_eu_educat_1 = 9,
      non_eu_educat_2 = 11,
      non_eu_educat_3 = 13,
      educat_1_foreign = 15,
      educat_2_foreign = 17,
      educat_3_foreign = 19,
      educat_1_native = 21,
      educat_2_native = 23,
      educat_3_native = 25
    ) |>
    mutate(
      across(-city_name, as.numeric),
      across(-c(year, city_name), ~ .x * 0.01)
    ) |>
    dplyr::filter(!is.na(year)) |>
    select(city_name, year, ends_with("foreign"))
}


complement_percent <- function(df_educ, detect_words, var_educat_1, var_educat_2, var_educat_3) {

  var_educat_1 <- enquo(var_educat_1)
  var_educat_2 <- enquo(var_educat_2)
  var_educat_3 <- enquo(var_educat_3)
  
  df_complement <- df_educ |>
    select(
      year, 
      city_name, 
      ends_with(detect_words)) |>
    mutate(
      !!sym(paste0("educat_1", "_", detect_words)) := if_else(is.na(!!var_educat_1), 1 - !!var_educat_2 - !!var_educat_3, !!var_educat_1),
      !!sym(paste0("educat_2", "_", detect_words)) := if_else(is.na(!!var_educat_2), 1 - !!var_educat_1 - !!var_educat_3, !!var_educat_2),
      !!sym(paste0("educat_3", "_", detect_words)) := if_else(is.na(!!var_educat_3), 1 - !!var_educat_1 - !!var_educat_2, !!var_educat_3)
    ) 
}


merge_cores_data <- function(df_german_foreign, df_corr) {

  df <- df_german_foreign |> 
    left_join(df_corrs, by = c("county_name" = "county_name")) |>
    select(
      nuts_code3,
      year,
      city_name,
      nuts_code2,
      total = closing_stock
    )
}


aggregate_population_by_nuts2 <- function(df) {

  df_output <- df |>
    summarise(
      total = sum(total, na.rm = TRUE), 
      .by = c(nuts_code2, city_name, year)
      )

  return(df_output)
}


merge_educ_data <- function(df_pop_de, df_educ) {

  df_merge <- df_pop_de |> 
    left_join(df_educ, by = c("nuts_code2", "city_name", "year")) |> View()
    mutate(
      across(ends_with(c("educat_1", "educat_2", "educat_3")), ~.*0.01)
    )

  return(df_merge)
}


change_percent_to_level <- function(df_educ, df_de_age) {

  df_educ |> 
    left_join(df_de_age, by = c("city_name", "year")) |>
    # Age from 24 - 65 data is only available in 2011 and 2022
    dplyr::filter(
      year %in% c(2011, 2022),
      # This city is not abailable because of the stats reason
      city_name != "Saarland" 
    ) |> 
    mutate(
      lv_edu_1 = educat_1_foreign * y25_64,
      lv_edu_2 = educat_2_foreign * y25_64,
      lv_edu_3 = educat_3_foreign * y25_64,
      across(starts_with("lv"), round)
    ) 

}
