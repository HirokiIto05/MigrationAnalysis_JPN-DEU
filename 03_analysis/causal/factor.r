main <- function() {
  source(here("03_analysis", "analysis_funs.r"))

  # Load data
  ### Main data
  df_jp_raw <- read.csv(here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932") |>
    dplyr::filter(between(year, 2013, 2019))
  df_de_raw <- read.csv(here("01_data", "intermediate", "german", "foreign_master.csv")) |>
    dplyr::filter(between(year, 2013, 2019))

  df_jp_native_raw <- read.csv(here("01_data", "intermediate", "population", "japanese_master.csv"), fileEncoding = "cp932") |>
    dplyr::filter(between(year, 2013, 2019))
  df_de_native <- read.csv(here("01_data", "intermediate", "german", "native_master.csv")) |>
    select(-total_foreign, -county_name) |>
    rename(native = population) |>
    dplyr::filter(between(year, 2013, 2019)) 

  df_jp <- select_cols_jp(df_jp_raw) |>
    create_scatter_df(total)
  df_de <- select_cols_de(df_de_raw) |>
    create_scatter_df(total)
  df_jp_native <- select_cols_jp(df_jp_native_raw) |>
    create_scatter_df(total)

  ### Age data
  # df_de_age <- read.csv(here("01_data", "intermediate", "german", "age_muni_master.csv")) |> View()

  ### wage data(local)
  df_de_wage <- read.csv(here("01_data", "intermediate", "german", "wage.csv")) |>
    select(county_id, year, wage = value) |>
    dplyr::filter(between(year, 2013, 2019)) 

  ### gender data
  df_de_gender <- read.csv(here("01_data", "intermediate", "german", "gender.csv")) |> 
    mutate(female_rate = female / total) |>
    select(year, county_id, female_rate)

  ### class data
  df_de_class <- read.csv(here("01_data", "intermediate", "german", "class.csv")) |>
    mutate(
      is_big = if_else(class == "big", 1, 0),
      is_urban = if_else(class == "urban", 1, 0)
    )

  ### Language data
  df_dist <- readxl::read_xls(here("01_data", "raw", "lang", "geo_cepii.xls"))

  ### continent data
  df_de_continent <- read_xlsx(here("01_data", "intermediate", "german", "population_region.xlsx")) |> 
    dplyr::filter(between(year, 2013, 2019)) |> 
    pivot_wider(
      names_from = region,
      values_from = population
    ) |>
    janitor::clean_names() |> 
    select(
      county_id, year, 
      europe, 
      africa, 
      north_america, 
      south_america, 
      asia, 
      australia_and_oceania
    )


  # Merge data
  df <- merge_data(
    df_de, 
    df_de_native, 
    df_de_wage,
    df_de_gender,
    df_de_class,
    df_de_continent)
  
}

merge_data <- function(
  df_de, df_de_native, df_de_wage,
  df_de_gender, df_de_class, df_de_continent) {

    df_output <- df_de |>
      left_join(df_de_native, by = c("year", "county_id")) |>
      left_join(df_de_wage, by = c("year", "county_id")) |>
      left_join(df_de_gender, by = c("year", "county_id")) |>
      left_join(df_de_class, by = "county_id") |>
      left_join(df_de_continent, by = c("year", "county_id")) |>
      mutate(
        europe_rate = europe / total,
      )

}



#' @title execute a basic lm model
#' @param df_input Dataframe : input data
#' @param var_y String : outcome variable
#' @param var_x String : regressor variable
#' @description Create a lm model for each year
create_fe <- function(df_input, var_y, var_x) {

  df_fe <- df |>  
    dplyr::filter(!is.na(ln_change_rate_total))

  df_rural <- df_fe |>
    dplyr::filter(class == "rural")

  df_urban <- df_fe |>
    dplyr::filter(class == "urban")


  formula_fe <- paste0(
    "ln_change_rate_total", "~", 
    "wage", "+",
    "is_big", "+",
    "is_urban", "+",
    "female_rate", "+",
    "europe_rate",
    "|", "year + county_id"
    )

  formula_base <- paste0(
    "ln_change_rate_total", "~", 
    "wage", "+",
    "is_big", "+",
    "is_urban", "+",
    "female_rate", "+",
    "europe_rate"
    )

  list_model <- list(
    "base" = fixest::feols(
      fml = as.formula(formula_base),
      data = df_fe
      # cluster = ~ county_id,
      # weights = ~ population
    ),
    "base_rural" = fixest::feols(
      fml = as.formula(formula_base),
      data = df_rural
      # cluster = ~ county_id,
      # weights = ~ population
    ),
    "base_urban" = fixest::feols(
      fml = as.formula(formula_base),
      data = df_urban
      # cluster = ~ county_id,
      # weights = ~ population
    ),
    "fe" = fixest::feols(
    fml = as.formula(formula_fe),
    data = df_fe
    # cluster = ~ county_id,
    # weights = ~ population
  )

  )

  return(model_output)
}



#' @title Create a model summary
#' @param model_input List : list of the result of lm_robust()
create_model_summary <- function (model_input, title_n) {
  
  gm <- tibble(
    raw = c("nobs", "r.squared", "adj.r.squared"),
    clean = c("N", "R2", "R2 adj"),
    fmt = c(0, 3, 3)
  )
  
  model_based <- modelsummary::modelsummary(model_input, fmt = "%.4f", 
                                        estimate =  "{estimate}{stars}",
                                        stars = c('*' = .1, '**' = .05, '***' = .01),
                                        # coef_rename = c("ln_lag_total" = "β1"),
                                        gof_map = gm,
                                        gof_omit = 'AIC|BIC|RMSE',
                                        coef_omit = 1:1,
                                        # output = "data.frame")
                                        output = "html")
  
  results_model <- model_based 
  
  return(results_model)
  
}

