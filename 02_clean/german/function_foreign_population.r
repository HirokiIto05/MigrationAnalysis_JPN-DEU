clean_population_data <- function(df_german_foreign) {

  df_output <- df_german_foreign |>
    select(
      date,
      county_id = city_code,
      total_value
    ) |>
    mutate(
      date = lubridate::ymd(date),
      year = lubridate::year(date)
    ) |>
    select(
      -date
    ) |>
    mutate(
      across(-c(year, county_id), as.numeric), 
      county_id = case_when(
        # merge
        county_id %in% c(3152, 3156) ~ 3159,
        county_id %in% c(16056, 16063) ~ 16063,
        .default = county_id
      )
    ) |>
    summarise(
      total_value = sum(total_value, na.rm = TRUE),
      .by = c(year, county_id)
    )

}




#' filter german data by category and county_id
#' 
#' @description category == Total
filter_german_category_and_id <- function(df) {

    df <- df |>
      janitor::clean_names() |> 
      dplyr::filter(
          category == "Total"
          # !str_detect(county_name, "until")
      ) |>  
      rename(
          county_id = id
      )

    return(df)
}


#' sum values
#' 
#' @description sum male and female values
sum_values <- function(df) {

        df <- df |> 
          mutate(
            across(-c(year, county_id, county_name, category), as.numeric), 
            county_id = case_when(
              # Mecklenburg-Vorpommern
              # https://sciencespo.hal.science/hal-01064470/document
              # Rostock and Schwerin don't change
              county_id %in% c(13055,13056,13052,13002) ~ 13071,
              county_id %in% c(13053,13051) ~ 13072,
              county_id %in% c(13057,13005,13061) ~ 13073,
              county_id %in% c(13057,13005,13061) ~ 13073,
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
          mutate(
            opening_stock
             = sum(c_across(starts_with("opening_stock")), na.rm = TRUE),
            additions 
             = sum(c_across(cols = c("additions_male", "additions_female")), na.rm = TRUE),
            additions_without_births
             = sum(c_across(cols = starts_with("additions_without_births")), na.rm = TRUE),
            addition_by_first_arrival_from_abroad
             = sum(c_across(cols = starts_with("addition_by_first_arrival_from_abroad")), na.rm = TRUE),
            addition_by_re_arrival_from_abroad
             = sum(c_across(cols = starts_with("addition_by_re_arrival_from_abroad")), na.rm = TRUE),
            addition_by_birth
             = sum(c_across(cols = starts_with("addition_by_birth")), na.rm = TRUE),
            addition_by_arr_from_oth_for_auth_same_reg_lev
             = sum(c_across(cols = starts_with("addition_by_arr_from_oth_for_auth_same_reg_lev")), na.rm = TRUE),
            exits
             = sum(c_across(cols = c("exits_male", "exits_female")), na.rm = TRUE),
            exits_without_deaths_and_deletions
             = sum(c_across(cols = starts_with("exits_without_deaths_and_deletions")), na.rm = TRUE),
            exit_by_departure_to_other_country
             = sum(c_across(cols = starts_with("exit_by_departure_to_other_country")), na.rm = TRUE),
            exit_by_ex_officio_deregistration
             = sum(c_across(cols = starts_with("exit_by_ex_officio_deregistration")), na.rm = TRUE),
            exit_by_death
             = sum(c_across(cols = starts_with("exit_by_death")), na.rm = TRUE),
            exit_by_dep_to_other_for_auth_of_same_reg_level
             = sum(c_across(cols = starts_with("exit_by_dep_to_other_for_auth_of_same_reg_level")), na.rm = TRUE),
            exit_by_deletion_from_central_reg_of_foreigners
             = sum(c_across(cols = starts_with("exit_by_deletion_from_central_reg_of_foreigners")), na.rm = TRUE),
            closing_stock
             = sum(c_across(cols = starts_with("closing_stock")), na.rm = TRUE),
            net_immigration_from_abroad
             = sum(c_across(cols = starts_with("net_immigration_from_abroad")), na.rm = TRUE),
            net_in_migr_from_within_germany_same_reg_level
             = sum(c_across(cols = starts_with("net_in_migr_from_within_germany_same_reg_level")), na.rm = TRUE),
            balance_of_cases_of_citizenship_acquired_lost
             = sum(c_across(cols = starts_with("balance_of_cases_of_citizenship_acquired_lost")), na.rm = TRUE),
            reclass_in_case_of_admin_district_reforms
             = sum(c_across(cols = starts_with("reclass_in_case_of_admin_district_reforms")), na.rm = TRUE),
            subsequently_registered_exits
             = sum(c_across(cols = starts_with("subsequently_registered_exits")), na.rm = TRUE),
            subsequently_registered_additions
             = sum(c_across(cols = starts_with("subsequently_registered_additions")), na.rm = TRUE),
            .by = c(year, county_id), .after = category
        ) |> 
        select(-ends_with(c("male", "female")))

    return(df)
}


add_variables <- function(df) {

  df_output <- df |> 
      select(
          county_id,
          year,
          county_name,
          opening_stock,
          additions,
          additions_without_births,
          addition_by_birth,
          addition_by_first_arrival_from_abroad,
          addition_by_re_arrival_from_abroad,
          addition_internal = addition_by_arr_from_oth_for_auth_same_reg_lev,
          closing_stock,
          exits,
          exit_by_death,
          exits_without_deaths_and_deletions,
          exit_by_departure_to_other_country,
          exit_by_ex_officio_deregistration,
          exit_internal = exit_by_dep_to_other_for_auth_of_same_reg_level,
          exit_by_deletion_from_central_reg_of_foreigners,
          net_immigration_international = net_immigration_from_abroad,
          net_migr_internal = net_in_migr_from_within_germany_same_reg_level
      ) |>
      arrange(county_id, year) |>
      mutate(
          addition_international = addition_by_first_arrival_from_abroad + addition_by_re_arrival_from_abroad,
          extis_international = exit_by_departure_to_other_country + exit_by_ex_officio_deregistration,
          total = closing_stock,
          change = (closing_stock - opening_stock),
          natural = (addition_by_birth - exit_by_death),
          social = change - natural,
          lag_total = dplyr::lag(total),
          change_rate = change/dplyr::lag(total)*100,
          natural_rate = natural/dplyr::lag(total)*100,
          social_rate = (social/dplyr::lag(total))*100,
          ln_total = log(total),
          ln10_total = log10(total),
          lag_ln_total = dplyr::lag(ln_total),
          lag_ln_total_5 = dplyr::lag(ln_total, n = 5),
          ln_change_rate_total = ln_total - lag_ln_total,
          ln_change_rate_total_5 = ln_total - lag_ln_total_5,
          .by = c(county_id)
      )
}




clean_register <- function(df) {

    df <- filter_german_category_and_id(df)

    df <- sum_values(df)
    df <- add_variables(df)

    return(df)
}



merge_population_data <- function(df_foreign_german, df_german_foreign_detail) {

  df_output <- df_german_foreign |>
    left_join(
      df_german_foreign_detail,
      by = c("county_id", "year")
    ) |>
    dplyr::filter(!str_detect(county_name, "until")) 
  
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
