generate_formula <- function(outcome_i, covariate_i){

  fomula_i <- paste0(outcome_i, " ~ ", covariate_i)

}

create_lm <- function(df_input, outcome_i, covariate_i) {

  formula_i <- generate_formula(outcome_i, covariate_i) |>
    as.formula()
  
  model_output <- list(
    "2014" = lm_robust(formula = formula_i, 
                       data = dplyr::filter(df_input, year == 2015)),
    "2015" = lm_robust(formula = formula_i, 
                       data = dplyr::filter(df_input, year == 2016)),
    "2016" = lm_robust(formula = formula_i, 
                       data = dplyr::filter(df_input, year == 2017)),
    "2017" = lm_robust(formula = formula_i, 
                       data = dplyr::filter(df_input, year == 2018)),
    "2018" = lm_robust(formula = formula_i, 
                       data = dplyr::filter(df_input, year == 2019)),
    "2019" = lm_robust(formula = formula_i, 
                       data = dplyr::filter(df_input, year == 2020)),
    "2020" = lm_robust(formula = formula_i, 
                       data = dplyr::filter(df_input, year == 2021)),
    "2021" = lm_robust(formula = formula_i, 
                       data = dplyr::filter(df_input, year == 2022))
    )
  
  
  
  return(model_output)
  
}

create_model_summary <- function (df, outcome_i, covariate_i) {

  df <- df |>
    filter(year > 2013) 

  num_outcome <- df |>
    pull(!!sym(outcome_i)) |>
    quantile(probs = c(0.01, 0.99), na.rm = TRUE)

  df <- df |>
    filter(
      between(!!sym(outcome_i), num_outcome[["1%"]], num_outcome[["99%"]])
    )

  model_input <- create_lm(df, outcome_i, covariate_i)
  
  gm <- tibble(
    raw = c("nobs", "r.squared", "adj.r.squared"),
    clean = c("N", "R2", "R2 adj"),
    fmt = c(0, 3, 3)
  )
  
  table_results <- modelsummary::msummary(model_input, fmt = "%.4f", 
                                        estimate =  "{estimate}{stars}",
                                        stars = c('*' = .1, '**' = .05, '***' = .01),
                                        coef_rename = c("ln_lag_total" = "β1"),
                                        gof_map = gm,
                                        gof_omit = 'AIC|BIC|RMSE',
                                        output = "data.frame")
  
  return(table_results)
  
}



output_table <- function(df, outcome_i, covariate_i) {

  gt_output <- create_model_summary(
    df = df_grm_native_master, 
    outcome_i =  "ln_change_rate", 
    covariate_i =  "lag(ln_population)"
  ) |>
  select(-part) |>
  gt() 
}
