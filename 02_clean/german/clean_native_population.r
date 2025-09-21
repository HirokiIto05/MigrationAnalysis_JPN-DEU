source(here("02_clean", "german", "function_clean_native.r"))

df_raw <- readxl::read_xlsx(
  here("01_data", "raw", "german", "native", "12411-0015_en.xlsx")
)

df_raw_foreign <- readxl::read_xlsx(
  here("01_data", "raw", "german", "foreign_residents", "12521-0040_en.xlsx")
)

df <- select_columns(df_raw) 
df <- merge_counties(df)
a_total <- df |> 
  summarise(
    population = sum(population, na.rm = TRUE),
    .by = c(year)
  )
a_foreign <-df_de |>
  summarise(
    total = sum(total, na.rm = TRUE),
    .by = c(year)
  )

a_total |>
  left_join(a_foreign, by = "year") |>
  mutate(
    native = population - total
  ) |> View()
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

write.csv(
  df,
  here("01_data", "intermediate", "german", "total_master.csv"), 
  row.names = FALSE)


df_foreign <- df_raw_foreign |> 
  select(
    date = 1,
    county_id = 2,
    county_name = 3,
    total = 6
  ) |> 
  fill(
    date, .direction = "down"
  ) |> 
  mutate(
    date = lubridate::ymd(date),
    year = lubridate::year(date)
  ) |> 
  select(
    -date
  ) |>
  drop_na(year) |>
  mutate(
    across(c(year, total, county_id), as.numeric), 
    county_id = case_when(
      # merge
      county_id %in% c(3152, 3156) ~ 3159,
      county_id %in% c(16056, 16063) ~ 16063,
      .default = county_id
    )
  ) |> 
  summarise(
    total_foreign = sum(total, na.rm = TRUE),
    .by = c(year, county_id)
  ) |> 
  dplyr::filter(total_foreign != 0)

df_output <- df |>
  left_join(df_foreign, by = c("county_id", "year")) |>
  mutate(
    population = population - total_foreign
  ) |>
  select(-total_foreign)

write.csv(
  df_output,
  here("01_data", "intermediate", "german", "native_master.csv"), 
  row.names = FALSE)

# 06611 Kassel, kreisfreie Stadt,
# 06633 Kassel, Landkreis:
# Data for the documenta city of Kassel ("Stadt") and the
# rural district of Kassel ("Landkreis") cannot be shown
# separately because they are compiled by a single foreigners
# authority.

# 10041 Regionalverband Saarbrücken, Landkreis,
# 10042 Merzig-Wadern, Landkreis,
# 10043 Neunkirchen, Landkreis,
# 10044 Saarlouis, Landkreis,
# 10045 Saarpfalz-Kreis,
# 10046 Sankt Wendel, Landkreis:
# The data for Saarland are not available by administrative
# district because there is just one foreigners authority in
# Saarland that is responsible for all administrative
# districts. All cases relating to Saarland appear under the
# heading for "Saarlouis, Landkreis", where the authority is
# located.

# 12052 Cottbus, kreisfreie Stadt,
# 12071 Spree-Neiße, Landkreis:
# Data for the city of Cottbus ("Stadt") and the rural
# district of Spree-Neiße ("Landkreis") cannot be shown
# separately because they are compiled by a single foreigners
# authority (from reference year 2013).