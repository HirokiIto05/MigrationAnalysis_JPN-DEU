source(here("02_clean", "german", "funs_merge_eurostat_main.r"))

# Load NUTS correspondence data
df_corr <- readxl::read_xlsx(
  here("01_data", "intermediate", "german", "nuts_correspondence.xlsx"))

# Load detailed foreign population data
df_german_foreign <- read.csv(here("01_data", "intermediate", "german", "foreign_master.csv")) |>
  dplyr::filter(!str_detect(county_name, "until")) |>
  dplyr::filter(
    !county_name %in% c(
      "Saarlouis, Landkreis",
      "Cottbus, kreisfreie Stadt"
      )
    )

df_main <- modify_german_reg_data(df_german_foreign, df_corr)
df_main <- aggregate_population_by_nuts2(df_main)



# Load Native population data
df_german_native <- read.csv(here("01_data", "intermediate", "german", "native_master.csv")) |>
  rename(
    native_total = population 
  )

df_native <- modify_german_native_data(df_german_native)
df_native_nuts2 <- aggregate_native_population_by_nuts2(df_native)

df_main <- df_main |>
  left_join(df_native_nuts2, by = c("nuts_code2", "city_name", "year"))

# Load educational attainment data
df_educ <- readxl::read_xlsx(
  here("01_data", "intermediate", "german", "educ_master.xlsx")) |>
  left_join(df_corr |> distinct(city_name, nuts_code2), by = "city_name")

# Load residence permit data
df_res_pmt <- read.csv(here("01_data", "intermediate", "german", "selected_residence_permit.csv")) |>
  select(-X)

df_res_pmt <- df_res_pmt |>  
  left_join(df_corr, by = c("county_name" = "original_name")) |> View()


# Merge data frames
df_merge <- df_main |>
  left_join(df_educ, by = c("nuts_code2", "city_name", "year")) |>  
  mutate(
    across(ends_with(c("educat_1", "educat_2", "educat_3")), ~.*0.01)
  )

# Save merged data
openxlsx::write.xlsx(
  df_merge,
  here("01_data", "intermediate", "german", "master_nuts2.xlsx"))


