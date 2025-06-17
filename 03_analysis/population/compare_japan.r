main <- function() {

    df_foreign <- read.csv(here::here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932") |>  
      dplyr::filter(year <= 2020)
    
    df_japan <- read.csv(here::here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932") |>
      dplyr::filter(year <= 2020)

}


add_variables <- function(df_foreign, df_japan) {

    df_output <- df_japan |>
        dplyr::bind_rows(df_foreign) |>
        select(city_id, year, total, moving_in_dom, moving_in_int) |>
        mutate(
            population_all = sum(total, na.rm = TRUE),
            .by = c(city_id, year)
        )

    df_output <- df_output |>
        mutate(
            rate_from_domestic = moving_in_dom / population_all,
            rate_from_abroad = moving_in_int / population_all
        ) |>
        summarise(
            rate_from_domestic = mean(rate_from_domestic, na.rm = TRUE),
            rate_from_abroad = mean(rate_from_abroad, na.rm = TRUE),
            .by = c(city_id)
        )
    
}



ggplot(df_output, aes(x = rate_from_domestic, y = rate_from_abroad)) +
    geom_point() +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 5)
        ) +
    labs(
        x = "Rate of moving in from domestic",
        y = "Rate of moving in from abroad "
        ) +
        


