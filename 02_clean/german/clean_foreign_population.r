# ----------------------------------- Setup ---------------------------------- #

source(here("02_clean", "german", "function_foreign_population.r"))
source(here("06_administration", "basic_packages.r"))


# --------------------------------- Read Data -------------------------------- #
df_german_foreign_raw <- read.csv(here::here("01_data", "raw", "german", "foreign", "population.csv"))
df_german_foreign_detail_raw <- read.csv(here::here("01_data", "raw", "german", "foreign", "register.csv"))
# --------------------------------- Clean Data -------------------------------- #

df_german_foreign <- clean_population_data(df_german_foreign_raw) |>
    remove_unavailable_counties()
df_german_foreign_detail <- clean_register(df_german_foreign_detail_raw) |>
    remove_unavailable_counties()

# limit valid counties
valid_counties <- df_german_foreign |> 
  distinct(county_id) |> 
  pull()

df_german_foreign_detail <- df_german_foreign_detail |>
  dplyr::filter(county_id %in% valid_counties)

df_german_foreign_detail |> select(year, county_id, county_name, additions_without_births, exits_without_deaths_and_deletions, addition_internal, addition_international, exit_internal, extis_international, starts_with("net")) |> View()

df_output <- merge_population_data(df_foreign_german, df_german_foreign_detail) 

write_csv(
    df_output,
    here("01_data", "intermediate", "german", "foreign_master.csv")
    )
