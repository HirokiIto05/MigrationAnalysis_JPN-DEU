main <- function() {
  
  df_jap <- read.csv(here::here("01_data", "intermediate", "population", "japanese.csv"), fileEncoding = "cp932") |>
    dplyr::ungroup() |> 
    dplyr::filter(year != 2013)
  
  df_for <- read.csv(here::here("01_data", "intermediate", "population", "overseas.csv"), fileEncoding = "cp932") |> 
    dplyr::ungroup() |> 
    dplyr::filter(year != 2013) |> 
    dplyr::filter(lag_ln_total > -Inf) |> 
    dplyr::filter(ln_change_rate_total > -Inf) 
    
  
  model_jap <- create_lm(df_jap)
  show_jap <- create_model_summary(model_jap, "日本人")

  model_for <- create_lm(df_for)
  model_for |>modelsummary(stars = TRUE)
  show_for <- create_model_summary(model_for, "外国人")

  openxlsx::write.xlsx(show_jap, file = here::here("04_output", "tables", "table_5_japan.xlsx"))
  openxlsx::write.xlsx(show_for, file = here::here("04_output", "tables", "table_5_foreign.xlsx"))
  
}

create_lm <- function(df_input) {
  
  model_output <- list(
    "2014" = lm_robust(formula = ln_change_rate_total ~  lag_ln_total, 
                       data = dplyr::filter(df_input, year == 2015)),
    "2015" = lm_robust(formula = ln_change_rate_total ~  lag_ln_total, 
                       data = dplyr::filter(df_input, year == 2016)),
    "2016" = lm_robust(formula = ln_change_rate_total ~  lag_ln_total, 
                       data = dplyr::filter(df_input, year == 2017)),
    "2017" = lm_robust(formula = ln_change_rate_total ~  lag_ln_total, 
                       data = dplyr::filter(df_input, year == 2018)),
    "2018" = lm_robust(formula = ln_change_rate_total ~  lag_ln_total, 
                       data = dplyr::filter(df_input, year == 2019)),
    "2019" = lm_robust(formula = ln_change_rate_total ~  lag_ln_total, 
                       data = dplyr::filter(df_input, year == 2020)),
    "2020" = lm_robust(formula = ln_change_rate_total ~  lag_ln_total, 
                       data = dplyr::filter(df_input, year == 2021)),
    "2021" = lm_robust(formula = ln_change_rate_total ~  lag_ln_total, 
                       data = dplyr::filter(df_input, year == 2022))
    )
  
  
  
  return(model_output)
  
}

create_model_summary <- function (model_input, title_n) {
  
  gm <- tibble(
    raw = c("nobs", "r.squared", "adj.r.squared"),
    clean = c("N", "R2", "R2 adj"),
    fmt = c(0, 3, 3)
  )
  
  model_based <- modelsummary::msummary(model_input, fmt = "%.4f", 
                                        estimate =  "{estimate}{stars}",
                                        stars = c('*' = .1, '**' = .05, '***' = .01),
                                        coef_rename = c("ln_lag_total" = "β1"),
                                        gof_map = gm,
                                        gof_omit = 'AIC|BIC|RMSE',
                                        output = "data.frame")
  
  results_model <- model_based 
  
  return(results_model)
  
}


main()
