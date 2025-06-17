#' @description Select columns for Japan data
select_cols_jp <- function(df_jp) {

  df_output <- df_jp |>
    select(
      county_id = city_id, county_name = city_name, prefecture_name, year, total,
      add = moving_in_total,
      add_internal = moving_in_dom,
      add_external = moving_in_int,
      add_others = moving_in_others,
      birth,
      exit = moving_out_total,
      exit_internal = moving_out_dom,
      exit_external = moving_out_int,
      exit_others = moving_out_others,
      death = mortality
    )
}

#' @description Select columns for Japan data
select_cols_de <- function(df_de) {

  df_output <- df_de |>
    select(
      county_id, county_name, year, total = total_value,
      add = additions,
      add_internal = addition_internal,
      add_external = addition_international,
      birth = addition_by_birth,
      exit = exits,
      exit_internal = exit_internal,
      exit_external = extis_international,
      death = exit_by_death
    )
}

#' @title Create analysis data
#' @description Add a regressor and outcome with lagged data.
#' At that time, winsorize the top and bottom 1%
create_scatter_df <- function(input_df, var_y, cutoff = 1, lag_i = 1) {

  var_y <- enquo(var_y)
  
  plot_based_df <- input_df |> 
    arrange(county_id, year) |> 
    mutate(
      ln_total = log(!!var_y),
      lag_ln_total = dplyr::lag(log(!!var_y), n = lag_i),
      # ln_change_rate_total = log(!!var_y) - dplyr::lag(log(!!var_y), n = lag_i),
      ln_change_rate_total = ln_total - lag_ln_total,
      .by = county_id
    ) |>  
    mutate(ln_change_rate_total = Winsorize(ln_change_rate_total, val = quantile(ln_change_rate_total, probs = c(0.01, 0.99), na.rm = TRUE)), .by = year)  |> 
    dplyr::slice_max(prop = cutoff, order_by = total, by = "year")
}

#' @title Create a basic scatter plot
#' 
#' @param lag_i The number of lags
create_scatter <-  function(input_df, var_x){

  var_x <- enquo(var_x)

  output_plot <- ggplot(input_df,
                       aes(x = !!var_x, y = ln_change_rate_total)) +
    geom_point(alpha = 0.5, colour = "#333333",
               fill = "#333333") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(
      panel.border       = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      axis.line.y.left   = element_line(color = 'black'),
      axis.line.y.right  = element_line(color = 'black'),
      axis.text.y.right  = element_blank(),
      plot.title = element_text(size = 15),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      panel.grid.major.y = element_line(color = "lightgray"),
      strip.background = element_blank(),
      strip.text = element_text(size = 22),
      strip.text.x = element_text(size = 20)
      ) +
    geom_hline(yintercept = 0,
               linewidth = 0.6,
               colour = "black",
               linetype = "solid") +
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                color = "#3C8DAD",
                linewidth = 1.3) +
    facet_wrap(~ year,
               scales = "free") 
    # scale_x_continuous(breaks = c(5, 10)) +
    # scale_y_continuous(
    #   breaks = c(-2, -1, 0, 1, 2),
    #   limits = c(-2, 2)
    # )
  
  return(output_plot)
}


#' @title Create a basic scatter plot
#' 
#' @param lag_i The number of lags
create_scatter_int <-  function(input_df, var_x, var_y){

  var_x <- enquo(var_x)
  var_y <- enquo(var_y)

  output_plot <- ggplot(input_df,
                       aes(x = !!var_x, y = !!var_y)) +
    geom_point(alpha = 0.5, colour = "#333333",
               fill = "#333333") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(
      panel.border       = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      axis.line.y.left   = element_line(color = 'black'),
      axis.line.y.right  = element_line(color = 'black'),
      axis.text.y.right  = element_blank(),
      plot.title = element_text(size = 15),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      panel.grid.major.y = element_line(color = "lightgray"),
      strip.background = element_blank(),
      strip.text = element_text(size = 22),
      strip.text.x = element_text(size = 20)
      ) +
    geom_hline(yintercept = 0,
               linewidth = 0.6,
               colour = "black",
               linetype = "solid") +
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                color = "#3C8DAD",
                linewidth = 1.3) +
    facet_wrap(~ year,
               scales = "free") 
    # scale_x_continuous(breaks = c(5, 10)) +
    # scale_y_continuous(
    #   breaks = c(-2, -1, 0, 1, 2),
    #   limits = c(-2, 2)
    # )
  
  return(output_plot)
}


#' @title execute a basic lm model
#' @param df_input Dataframe : input data
#' @param var_y String : outcome variable
#' @param var_x String : regressor variable
#' @description Create a lm model for each year
create_lm <- function(df_input, var_y, var_x) {

  df <- df_input

  formula_i <- paste0(var_y, "~", var_x)
  
  model_output <- list(
    "2014" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2014)),
    "2015" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2015)),
    "2016" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2016)),
    "2017" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2017)),
    "2018" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2018)),
    "2019" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2019)),
    "2020" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2020)),
    "2021" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2021)),
    "2022" = lm_robust(formula = as.formula(formula_i), 
                       data = dplyr::filter(df, year == 2022))
    # "2023" = lm_robust(formula = as.formula(formula_i), 
    #                     data = dplyr::filter(df, year == 2023))
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


#' @title change countrys' name to adjust other datasets
#' @param df_country Dataframe : country master data
#' 
#' @description Change country names of the main df 
#' to match the dataset which define the level of countries.
change_country_name <- function(df) {

  df_output <- df |>
  dplyr::filter(
    between(year, 2013, 2019)
    ) |>
  mutate(
    country_name = case_when(
      country_name == "Egypt" ~ "Egypt, Arab Rep.",
      country_name == "Afghanistan, Republic of" ~ "Afghanistan",
      country_name == "Bahamas" ~ "Bahamas, The",
      country_name == "Bolivia, Plurinational State" ~ "Bolivia",
      country_name == "Gambia" ~ "Gambia, The",
      country_name == "Hong Kong" ~ "Hong Kong SAR, China",
      country_name == "Iran, Islamic Republic of" ~ "Iran, Islamic Rep.",
      country_name == "Yemen" ~ "Yemen, Rep.",
      country_name == "Kyrgyzstan" ~ "Kyrgyz Republic",
      country_name == "Congo, The Democratic Republic of the" ~ "Congo, Dem. Rep.",
      country_name == "Congo, Republic of the" ~ "Congo, Rep.",
      country_name == "Korea, Democratic People's Republic" ~ "Korea, Dem. Rep.",
      country_name == "Korea, Republic of" ~ "Korea, Rep.",
      country_name == "Lao People's Democratic Republic" ~ "Lao PDR",
      country_name == "Macau" ~ "Macao SAR, China",
      country_name == "Micronesia, Federated States of" ~ "Micronesia, Fed. Sts.",
      country_name == "Moldova, Republic of" ~ "Moldova",
      country_name == "Montenegro (since 2006-06-03)" ~ "Montenegro",
      # country_name == "Palestinian Territories" ~ "Other",
      # country_name == "Serbia (incl. Kosovo) (2006-06-03 to 2008-02-16)" ~ "Other",
      # country_name == "Serbia and Montenegro (2003-02-05 to 2006-06-02)" ~ "Other",
      country_name == "Slovakia" ~ "Slovak Republic",
      # country_name == "Soviet Union (until 1991-12-25)" ~ "Other",
      country_name == "Saint Kitts and Nevis" ~ "St. Kitts and Nevis",
      country_name == "Saint Lucia" ~ "St. Lucia",
      country_name == "Saint Vincent and the Grenadines" ~ "St. Vincent and the Grenadines",
      country_name == "Sudan (without South Sudan) (since 2011-07-09)" ~ "Sudan",
      country_name == "South Sudan (since 2011-07-09)" ~ "South Sudan",
      country_name == "Syrian" ~ "Syrian Arab Republic",
      country_name == "Taiwan" ~ "Taiwan, China",
      # country_name == "Czechoslovakia (until 1992-12-31)" ~ "Other",
      country_name == "Turkey" ~ "Trkiye",
      # country_name == "Vatican City State" ~ "Other",
      country_name == "Venezuela, Bolivarian Republic of" ~ "Venezuela, RB",
      # country_name == "Stateless" ~ "Other",
      # country_name == "Unknown / Not specified" ~ "Other",
      TRUE ~ country_name
    )
  )
}


remove_unavailable_counties <- function(df) {

  # Not available on Foreigner's data
  # Description below
  df <- df |>
  dplyr::filter(
    !county_id %in% c(
      # Saarland
      10041, # Regionalverband Saarbrücken, Landkreis
      10042, # Merzig-Wadern, Landkreis
      10043, # Neunkirchen, Landkreis
      10044, # Saarlouis, Landkreis
      10045, # Saarpfalz-Kreis
      10046  # Sankt Wendel, Landkreis
    ),
    !county_id %in% c(
      12052, # Cottbus, kreisfreie Stadt
      12071  # Spree-Neiße, Landkreis
    ),
    !county_id %in% c(
      06633 # Kassel, Landkreis
    )
  )
}


#' Merge municipalities
#' 
#' @description sum male and female values
merge_municipalities <- function(df) {

        df <- df |> 
          mutate(
            # across(-c(year, county_id, county_name, category), as.numeric), 
            county_id = case_when(
              # Mecklenburg-Vorpommern
              # https://sciencespo.hal.science/hal-01064470/document
              # Rostock and Schwerin don't change
              county_id %in% c(13055,13056,13052,13002) ~ 13071,
              county_id %in% c(13053,13051) ~ 13072,
              county_id %in% c(13057,13005,13061) ~ 13073,
              county_id %in% c(13057,13005,13061) ~ 13073,
              county_id %in% c(13058,13006) ~ 13074,
              county_id %in% c(13001,13059,13062) ~ 13075,
              county_id %in% c(13054,13060) ~ 13076,
              # Gettingen
              county_id %in% c(3152, 3156) ~ 3159,
              # Eisenach
              county_id %in% c(16056, 16063) ~ 16063,
              .default = county_id
              )
          )

    return(df)
}


