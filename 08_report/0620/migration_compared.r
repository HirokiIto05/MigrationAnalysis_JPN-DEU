df_foreign <- read.csv(here::here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932")



df_japan <- read.csv(here::here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932")

df_total <- df_japan |>
    dplyr::bind_rows(df_foreign) |>
    select(city_id, year, total) |>
    summarise(
        population_all = sum(total, na.rm = TRUE),
        .by = c(city_id, year)
    )


df_foreign <- df_foreign |>
    left_join(df_total, by = c("city_id", "year")) |>
    dplyr::mutate(
        dom_rate = moving_in_dom / population_all,
        int_rate = moving_in_int / population_all
    )
df_foreign |> View()


df_foreign |>
    dplyr::summarise()