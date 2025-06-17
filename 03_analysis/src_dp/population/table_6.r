main <- function() {
  
  df_jap <- read.csv(here::here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932")  |>
    dplyr::ungroup() |> 
    dplyr::filter(year != 2013)

  df_for <- read.csv(here::here("01_data", "intermediate", "population", "overseas_master.csv"),  fileEncoding = "cp932") |> 
    dplyr::ungroup() |> 
    dplyr::filter(year != 2013) |> 
    dplyr::filter(lag_ln_total > -Inf) |> 
    dplyr::filter(social_rate > -50 & social_rate < 50) |> 
    relocate(ln_total, .after = total)  
  
  model_jap <- create_lm(df_jap)
  model_for <- create_lm(df_for)
  
  show_jap <- create_model_summary(model_jap, "日本人")
  show_for <- create_model_summary(model_for, "外国人(X軸外国人人口)")
  
  # エクセル形式での保存
  write.xlsx(show_jap, file = here::here("04_output", "tables", "output_dp", "table_6_japanese.xlsx"))
  write.xlsx(show_for, file = here::here("04_output", "tables", "output_dp", "table_6_foreign.xlsx"))
}


create_lm <- function(df_input, top_n) {
  
  model_output <- list(
    "2014" = estimatr::lm_robust(formula = social_rate ~ lag_ln_total, data = dplyr::filter(df_input, year == 2015)),
    "2015" = estimatr::lm_robust(formula = social_rate ~ lag_ln_total, data = dplyr::filter(df_input, year == 2016)),
    "2016" = estimatr::lm_robust(formula = social_rate ~ lag_ln_total, data = dplyr::filter(df_input, year == 2017)),
    "2017" = estimatr::lm_robust(formula = social_rate ~ lag_ln_total, data = dplyr::filter(df_input, year == 2018)),
    "2018" = estimatr::lm_robust(formula = social_rate ~ lag_ln_total, data = dplyr::filter(df_input, year == 2019)),
    "2019" = estimatr::lm_robust(formula = social_rate ~ lag_ln_total, data = dplyr::filter(df_input, year == 2020)),
    "2020" = estimatr::lm_robust(formula = social_rate ~ lag_ln_total, data = dplyr::filter(df_input, year == 2021)),
    "2021" = estimatr::lm_robust(formula = social_rate ~ lag_ln_total, data = dplyr::filter(df_input, year == 2022))
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
