df_jp <- read.csv(here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932") |>  
select(city_id, city_name, prefecture_name, year, total_foreign  = total, change_foreign = change)
df_jp_native <- read.csv(here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932") |> 
select(city_id, city_name, prefecture_name, year, total_native = total, change_native = change)

renv::paths$cache()

df_jp |>
  left_join(df_jp_native, by = c("city_id", "city_name", "prefecture_name", "year")) |> 
  mutate(complement = change_native + change_foreign) |> 
  dplyr::filter(year == 2014) |>
  dplyr::filter(change_native < 0, complement > 0) 
  # select(city_id, city_name, prefecture_name, year, total_foreign, total_native, complement) |> View()

df_de <- read.csv(here("01_data", "intermediate", "german", "foreign_master.csv")) |> 
  select(county_id, county_name, year, total_foreign = total_value, change_foreign = change)
df_de_native <- read.csv(here("01_data", "intermediate", "german", "native_master.csv")) |> 
  select(county_id, county_name, year, total_native = population) |>
  mutate(
    change_native = total_native - dplyr::lag(total_native)
  )


df_de |>
  left_join(df_de_native, by = c("county_id", "county_name", "year")) |> 
  mutate(complement = change_native + change_foreign) |> 
  dplyr::filter(year == 2019) |>
  dplyr::filter(change_native < 0, complement > 0)

Sys.getenv("RENV_PATHS_CACHE")
Sys.unsetenv("RENV_PATHS_CACHE")

Sys.getenv("RENV_PATHS_CACHE")
