main <- function() {
  source(here("03_analysis", "analysis_funs.r"))
  
  # Load data
  df_de_total <- read.csv(here("01_data", "intermediate", "german", "total_master.csv"))

  df_de_area <- readxl::read_xlsx(here("01_data", "raw", "german", "area",  "area.xlsx")) |>
  select(
    county_id = 1,
    area = 3
  ) |>
  mutate(across(c(area, county_id), as.numeric)) |>
  dplyr::filter(!is.na(area))
  
  # Create class data
  df_de_class <- df_de_total |> 
  rename(total = population) |>
  left_join(df_de_area, by = "county_id") |> 
  mutate(density = total / area) |> 
  dplyr::filter(year == 2019) |>
  mutate(
    class = case_when(
      total >= 500000 ~ "big",
      (density >= 150) & (total >=100000)  ~ "urban",
      # density >= 100 ~ "mid",
      # density < 100 ~ "rural",
      TRUE ~ "rural"
    )
  ) |>
  select(county_id, class)
  
  # Save data
  write.csv(df_de_class, here("01_data", "intermediate", "german", "class.csv"), row.names = FALSE)
}