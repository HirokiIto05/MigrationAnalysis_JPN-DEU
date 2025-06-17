main <- function() {
  
  # read data -----------------------------------
  df_master <- read.csv(here::here("01_data", "intermediate", "foreign_status", "master.csv"), fileEncoding = "cp932")

  # estimates ------------------------------------
 
  # 3 years (2012 - 2015, 2016 - 2019, 2020 - 2022)
  # 外国人 総人口
  model_total_three <- lm_three_total(df_master)
  table_total_three <- create_lm_kable(model_total_three, "外国人　総人口") |>
    dplyr::filter(statistic != "N")
  # 特別永住者以外
  model_except_three <- lm_three_except(df_master)
  table_except_three <- create_lm_kable(model_except_three, "特別永住者以外")

  # 結合
  table_adjusted <- table_total_three |>
    dplyr::bind_rows(table_except_three)

  write.xlsx(table_adjusted, file = here::here("04_output", "tables", "table_7.xlsx"))


  # 5 years
  # model_except_five <- lm_five_except(df_master)
  # model_except_five
  # table_except_five <- create_lm_kable(model_except_five, "特別永住者以外")

  # write.xlsx(table_except_five, file = "04_analyze/foreign_status/table/modified/except_five.xlsx")


  # model_total_five <- lm_five_total(df_master)
  # table_total_five <- create_lm_kable(model_total_five, "外国人　総人口")
  
  # write.xlsx(table_total_five, file = "04_analyze/foreign_status/table/modified/total_five.xlsx")
}


create_lm_kable <- function (model_input, title_n) {

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

  df_adjusted <- adjust_table(df_estimates, title_n)
    
  return(df_adjusted)
  
}

df_input <- df_master
df_input |> dplyr::filter(between(year, 2020, 2022)) |> View()

lm_three_total <- function(df_input) {
  
  list_output <- list(
  
  "2012 - 2015" = lm_robust(formula = ln_change_rate_foreign_3 ~  lag_ln_foreign_3,
                            data = dplyr::filter(df_input, year == 2015)),
  "2016 - 2019" = lm_robust(formula = ln_change_rate_foreign_3 ~  lag_ln_foreign_3,
                            data = dplyr::filter(df_input, year == 2019)),
  "2020 - 2022" = lm_robust(formula = ln_change_rate_foreign_2 ~  lag_ln_foreign_2,
                            data = dplyr::filter(df_input, year == 2022))
  
  )

  return(list_output)
}


lm_three_except <- function(df_input) {
  
  list_output <- list(
    
    "2012 - 2015" = lm_robust(formula = ln_change_rate_except_specific_3 ~  lag_ln_except_specific_3,
                              data = dplyr::filter(df_input, year == 2015)),
    "2016 - 2019" = lm_robust(formula = ln_change_rate_except_specific_3 ~  lag_ln_except_specific_3,
                              data = dplyr::filter(df_input, year == 2019)),
    "2020 - 2022" = lm_robust(formula = ln_change_rate_except_specific_2 ~  lag_ln_except_specific_2,
                              data = dplyr::filter(df_input, year == 2022))
    
  )
  
  return(list_output)
}


lm_five_total <- function(df_input) {
  
  list_output <- list(
  
  "2010 - 2015" = lm_robust(formula = ln_change_rate_foreign_5 ~  lag_ln_foreign_5,
                            data = dplyr::filter(df_input, year == 2015)),
  "2015 - 2020" = lm_robust(formula = ln_change_rate_foreign_5 ~  lag_ln_foreign_5,
                            data = dplyr::filter(df_input, year == 2020)),
  "2010 - 2020" = lm_robust(formula = ln_change_rate_foreign_10 ~  lag_ln_foreign_10,
                            data = dplyr::filter(df_input, year == 2020))
  
  )
  
  return(list_output)
}


lm_five_except <- function(df_input) {
  
  list_output <- list(
    
    "2010 - 2015" = lm_robust(formula = ln_change_rate_except_specific_5 ~  lag_ln_except_specific_5,
                              data = dplyr::filter(df_input, year == 2015)),
    "2015 - 2020" = lm_robust(formula = ln_change_rate_except_specific_5 ~  lag_ln_except_specific_5,
                              data = dplyr::filter(df_input, year == 2020)),
    "2010 - 2020" = lm_robust(formula = ln_change_rate_except_specific_10 ~  lag_ln_except_specific_10,
                              data = dplyr::filter(df_input, year == 2020))
    
  )
  
  return(list_output)
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
          stringr::str_detect(term, "lag_ln"),
      ) |> 
      dplyr::mutate(
          "2020 - 2022" = dplyr::lead(!!sym("2020 - 2022"), n = 2)
      ) |> 
      dplyr::filter(
        !stringr::str_detect(term, "_2")
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