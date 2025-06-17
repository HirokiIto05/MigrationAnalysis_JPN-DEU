main <- function() {

  # Load data
  df_jp <- read.csv(here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932")
  df_de <- read.csv(here("01_data", "intermediate", "german", "foreign_master.csv"))

  df_jp <- select_cols_jp(df_jp)
  df_de <- select_cols_de(df_de)

  df_jp_native <- read.csv(here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932")
  df_de_native <- read.csv(here("01_data", "intermediate", "german", "native_master.csv"))

  # Save data
  # write.csv(data, "data/02_analyzed_data.csv", row.names = FALSE)

}


select_cols_jp <- function(df_jp) {

  df_output <- df_jp |>
    select(
      city_id, city_name, prefecture_name, year, total,
      add = moving_in_total,
      add_internal = moving_in_dom,
      add_external = moving_in_int,
      add_birth = birth,
      exit = moving_out_total,
      exit_internal = moving_out_dom,
      exit_external = moving_out_int,
      exit_death = mortality
    )
}


select_cols_de <- function(df_de) {

  df_output <- df_de |>
    select(
      county_id, county_name, year, total = total_value,
      add = additions,
      add_internal = addition_internal,
      add_external = addition_international,
      add_birth = addition_by_birth,
      exit = exits,
      exit_internal = exit_internal,
      exit_external = extis_international,
      exit_death = exit_by_death
    )
}


