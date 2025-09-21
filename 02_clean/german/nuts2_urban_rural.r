df_raw <- readxl::read_xlsx(
  here("01_data", "raw", "german", "eurostat", "NUTS2021.xlsx"),
  sheet = "Urban-rural"
)


# Based on the EUROSTAT, there are degree of urbanisation (DEGURBA) classifications:

df <- df_raw |> 
  select(
    nuts_code = 1,
    nuts_class = 2,
    class_urban = 3
  ) |>
  dplyr::filter(str_sub(nuts_code, 1, 2) == "DE") |>
  mutate(
    class_urban = case_when(
      class_urban == "perdominantly urban" ~ "urban",
      class_urban == "intermediate" ~ "intermediate",
      class_urban == "predominantly rural" ~ "rural",
      .default = class_urban
    ))


write.csv(
  df,
  here("01_data", "intermediate", "german", "nuts2_urban_rural.csv"),
  row.names = FALSE
)

df_corres <- readxl::read_xlsx(
  here("01_data", "intermediate", "german", "nuts_correspondence.xlsx")
)
