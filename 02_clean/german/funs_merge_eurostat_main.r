modify_german_reg_data <- function(df_german_foreign, df_corr) {

  df <- df_german_foreign |>
    left_join(df_corr, by = c("county_name" = "county_name")) |>
    select(
      county_name,
      nuts_code3,
      year,
      city_name,
      nuts_code2,
      everything()
    ) |>
    select(-county_name)
}


aggregate_population_by_nuts2 <- function(df) {

  df_main |>
  # df_output <- df |>
    reframe(
      across(
        c(
          total,
          opening_stock,
          additions,
          additions_without_births,
          addition_by_birth,
          closing_stock,
          exits,
          exits_without_deaths_and_deletions,
          exit_by_death),
          ~sum(.x, na.rm = TRUE)), .by = c(nuts_code2, city_name, year)) |> View()

  return(df_output)
}

modify_german_native_data <- function(df_german_native) {

  df <- df_german_native |>
    left_join(df_corr, by = c("county_name" = "original_name")) |>
    select(
      # -county_name,
      nuts_name,
      nuts_code,
      year,
      city_name,
      nuts_code2,
      everything()
    ) |>
    select(-county_name)
}


aggregate_native_population_by_nuts2 <- function(df) {

  df_output <- df |>
    summarise(
      native_total = sum(native_total, na.rm = TRUE), 
      .by = c(nuts_code2, city_name, year)
      )

}


aggregate_rep_pmt <- function(df_res_pmt) {

  df_output <- df_res_pmt |>
    reframe(
      across(
        c(),
          ~sum(.x, na.rm = TRUE)), .by = c(nuts_code2, city_name, year))
}
