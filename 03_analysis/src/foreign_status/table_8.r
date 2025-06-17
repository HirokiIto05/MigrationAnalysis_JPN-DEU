 main <- function() {
  
  # read data -----------------------------------
  df_master <- read.csv(here::here("01_data", "intermediate", "foreign_status", "master.csv"), fileEncoding = "cp932")
  
  # estimate ------------------------------------
  model_high <- estimate_high(df_master)
  model_low <- estimate_training(df_master) 
  model_status_total <- estimate_status(df_master)

  table_high <- create_lm_table(model_high, "高技能資格者", 4) |>
    dplyr::filter(statistic != "N")
  table_low <- create_lm_table(model_low, "技能実習・特定技能　合計", 4) |>
    dplyr::filter(statistic != "N")
  table_status <- create_lm_table(model_status_total, "身分系資格者（特別永住者含む）　合計", 4)

  table_output <- table_high |>
    dplyr::bind_rows(table_low) |>
    dplyr::bind_rows(table_status)

  # save ----------------------------------------
  write.xlsx(table_output, file = here::here("04_output", "tables", "output_dp", "table_8.xlsx"))

}


estimate_high <- function(df_input) {
  
  list_output <- list(
    
    "2012 - 2015" = lm_robust(formula = ln_change_rate_high_3 ~  lag_ln_high_3,
                              data = dplyr::filter(df_input, year == 2015)),
    "2016 - 2019" = lm_robust(formula = ln_change_rate_high_3 ~  lag_ln_high_3,
                              data = dplyr::filter(df_input, year == 2019)),
    "2020 - 2022" = lm_robust(formula = ln_change_rate_high_2 ~  lag_ln_high_2,
                              data = dplyr::filter(df_input, year == 2022)) 
    
  )
  
  return(list_output)
}


estimate_training <- function(df_input) {
  
  list_output <- list(
    
    "2012 - 2015" = lm_robust(formula = ln_change_rate_low_3 ~  lag_ln_low_3,
                              data = dplyr::filter(df_input, year == 2015)),
    "2016 - 2019" = lm_robust(formula = ln_change_rate_low_3 ~  lag_ln_low_3,
                              data = dplyr::filter(df_input, year == 2019)),
    "2020 - 2022" = lm_robust(formula = ln_change_rate_low_2 ~  lag_ln_low_2,
                              data = dplyr::filter(df_input, year == 2022))
    
  )
  
  return(list_output)
}


estimate_status <- function(df_input) {
  
  list_output <- list(
    
    "2012 - 2015" = lm_robust(formula = ln_change_rate_status_total_3 ~  lag_ln_status_total_3,
                              data = dplyr::filter(df_input, year == 2015)),
    "2016 - 2019" = lm_robust(formula = ln_change_rate_status_total_3 ~  lag_ln_status_total_3,
                              data = dplyr::filter(df_input, year == 2019)),
    "2020 - 2022" = lm_robust(formula = ln_change_rate_status_total_2 ~  lag_ln_status_total_2,
                              data = dplyr::filter(df_input, year == 2022))
    
  )
  
  return(list_output)
}


create_lm_table <- function (model_input, title_n, col_n) {
  
  gm <- tibble(
    raw = c("nobs", "r.squared", "adj.r.squared"),
    clean = c("N", "R2", "R2 adj"),
    fmt = c(0, 3, 3)
  )
  
  df_estimates <- modelsummary::msummary(model_input, fmt = "%.4f", 
                                        estimate =  "{estimate}{stars}",
                                        stars = c('*' = .1, '**' = .05, '***' = .01),
                                        coef_rename = c("ln_lag_total" = "β1"),
                                        gof_map = gm,
                                        output = "data.frame") 

  results_model <- adjust_table(df_estimates, title_n)

  return(results_model)
}


adjust_table <- function(df_estimates, title_n){

    df_base <- df_estimates |>
      dplyr::filter(
            !term %in% c("(Intercept)", "R2")
        ) |>
        dplyr::select(
            -part
        ) |>
        dplyr::mutate(
            dplyr::across(everything(), ~dplyr::na_if(., ""))
        )

    df_r2 <- df_base |>
        dplyr::filter(
            term %in% c("N", "R2 adj")
        ) |> 
        dplyr::mutate(
            statistic = term
        ) |>
        dplyr::select(-term)

    df_coef <- df_base |>
        dplyr::filter(
            term %in% c("lag_ln_high_3", "lag_ln_high_2")
        ) |>
    dplyr::mutate(
        "2020 - 2022" = dplyr::lead(!!sym("2020 - 2022"), n = 2)
    ) |> 
    dplyr::filter(
        term != "lag_ln_high_2"
    ) |>
    dplyr::select(-term)

    df_adjusted <- dplyr::bind_rows(df_coef, df_r2) |>
        dplyr::mutate(
            cateogry = title_n,
            .before = statistic
        )

    return(df_adjusted)

}


main()