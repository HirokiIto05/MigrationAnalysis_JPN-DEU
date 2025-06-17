main <- function() {

  list_years <- seq(2013, 2019)

  df_raw <- purrr::map(list_years, read_raw_data) |>
    bind_rows()

  df <- aggregate_population(df_raw)

  write.csv(df, here("01_data", "intermediate", "german", "selected_residence_permit.csv"))
}


read_raw_data <- function(year_i) {

  file_name_base_i <- here(
    "01_data",
    "raw",
    "german",
    "selected_residence_permit"
  ) 

  df_male <-  read.csv(paste0(file_name_base_i, "/", year_i, "m.csv"))
  df_female <-  read.csv(paste0(file_name_base_i, "/", year_i, "w.csv"))

  df_output <- df_male |>
    bind_rows(df_female)

}


aggregate_population <- function(df_raw) {

  df_output <- df_raw |>
    janitor::clean_names() |> 
    select(
      year = 1,
      county_id = 2,
      county_name = 3,
      country_name = 4,
      everything()
    ) |> 
    mutate(
      across(-c(year,county_id,county_name,country_name), as.numeric),
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
        ),
      county_name = if_else(county_id == 3159, "Göttingen, Landkreis", county_name),
      county_name = if_else(county_id == 16063, "Wartburgkreis", county_name)
    ) |>
    dplyr::filter(
      # Cottbus and Saarland are not available in the dataset
      !county_id %in% c(10044, 12052)
    ) |>
    summarise(
      right_of_resid_acc_to_eu_law_on_freedom_of_movem = sum(right_of_resid_acc_to_eu_law_on_freedom_of_movem, na.rm = TRUE),
      exempted_from_requirem_to_have_a_residence_title = sum(exempted_from_requirem_to_have_a_residence_title, na.rm = TRUE),
      unlimited_settlement_permit = sum(unlimited_settlement_permit, na.rm = TRUE),
      temporary_residence_permit = sum(temporary_residence_permit, na.rm = TRUE),
      # temporary_residence_permit_for_education_purposes = sum(temporary_residence_permit_for_education_purposes, na.rm = TRUE),
      # temporary_residence_permit_for_employment_purposes = sum(temporary_residence_permit_for_employment_purposes, na.rm = TRUE),
      temp_res_perm_for_reas_of_int_law_hum_pol_reasons = sum(temp_res_perm_for_reas_of_int_law_hum_pol_reasons, na.rm = TRUE),
      # temporary_residence_permit_for_family_reasons = sum(temporary_residence_permit_for_family_reasons, na.rm = TRUE),
      # temp_resid_permit_f_special_reasons_national_visas = sum(temp_resid_permit_f_special_reasons_national_visas, na.rm = TRUE),
      application_for_residence_title_filed = sum(application_for_residence_title_filed, na.rm = TRUE),
      temporary_suspension_of_deportation = sum(temporary_suspension_of_deportation, na.rm = TRUE),
      permission_to_reside = sum(permission_to_reside, na.rm = TRUE),
      no_res_title_temp_susp_of_dep_or_perm_to_reside = sum(no_res_title_temp_susp_of_dep_or_perm_to_reside, na.rm = TRUE),
      .by = c(year, county_id, county_name)
    ) |>
    mutate(
      total = rowSums(across(-c(year,county_id,county_name))),
      .after = county_name
      ) |>
    dplyr::filter(!is.na(county_id)) |>
    mutate(
      year = lubridate::year(year)
    ) |>
    dplyr::filter(total != 0) 
}
