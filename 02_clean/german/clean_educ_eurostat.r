# EUROSTAT 
# Detailed datasets -> Education and training outcomes -> 
# Educational attainment level -> Population by educational attainment level
# Population by educational attainment level, sex, age, citizenship and NUTS 2 region (%) (edat_lfs_9918)

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

  # write data
  openxlsx::write.xlsx(
    df_educ,
    here("01_data", "intermediate", "german", "educ_master.xlsx"))
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
