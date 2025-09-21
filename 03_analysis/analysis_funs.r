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
generate_df_main <- function(df_input, var_y, cutoff = 1, lag_i = 1) {

  var_y <- enquo(var_y)
  
  plot_based_df <- df_input |> 
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


#' @title Create analysis data
#' @description Add a regressor and outcome with lagged data.
#' At that time, winsorize the top and bottom 1%
generate_df_main_lag_n <- function(df_input, var_y) {

  var_y <- enquo(var_y)
  
  plot_based_df <- df_input |> 
    arrange(county_id, year) |> 
    mutate(
      ln_total = log(!!var_y),
      lag_ln_total2 = dplyr::lag(log(!!var_y), n = 2),
      lag_ln_total3 = dplyr::lag(log(!!var_y), n = 3),
      # ln_change_rate_total = log(!!var_y) - dplyr::lag(log(!!var_y), n = lag_i),
      ln_change_rate_total2 = ln_total - lag_ln_total2,
      ln_change_rate_total3 = ln_total - lag_ln_total3,
      .by = county_id
    ) |>  
    mutate(
      ln_change_rate_total2 = Winsorize(ln_change_rate_total2, val = quantile(ln_change_rate_total2, probs = c(0.01, 0.99), na.rm = TRUE)), 
      ln_change_rate_total3 = Winsorize(ln_change_rate_total3, val = quantile(ln_change_rate_total3, probs = c(0.01, 0.99), na.rm = TRUE)), 
      .by = year)  |> 
    mutate(
      ln_change_rate_total = case_when(
        year == 2015 ~ ln_change_rate_total2,
        year == 2019 ~ ln_change_rate_total3,
        year == 2023 ~ ln_change_rate_total3,
        .default = NA
      ),
      lag_ln_total = case_when(
        year == 2015 ~ lag_ln_total2,
        year == 2019 ~ lag_ln_total3,
        year == 2023 ~ lag_ln_total3,
        .default = NA
      )
    )
}


#' @title Create a basic scatter plot
#' 
#' @param lag_i The number of lags
create_scatter <-  function(input_df, var_x, title_i = NULL){

  var_x <- enquo(var_x)

  output_plot <- ggplot(input_df,
                       aes(x = !!var_x, y = ln_change_rate_total)) +
    geom_point(alpha = 0.5, colour = "#333333",
               fill = "#333333") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    labs(
      title = title_i,
      x = "Lagged log population",
      y = "Change rate of log population"
    ) +
    theme(
      panel.border       = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      axis.line.y.left   = element_line(color = 'black'),
      axis.line.y.right  = element_line(color = 'black'),
      axis.text.y.right  = element_blank(),
      plot.title = element_text(size = 15),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      panel.grid.major.y = element_line(color = "lightgray"),
      strip.background = element_blank(),
      strip.text = element_text(size = 20),
      strip.text.x = element_text(size = 18)
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
               scales = "fixed",
               ncol = 3) 
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
create_scatter_int <-  function(input_df, var_x, var_y, title_i){

  var_x <- enquo(var_x)
  var_y <- enquo(var_y)

  output_plot <- ggplot(input_df,
                       aes(x = !!var_x, y = !!var_y)) +
    geom_point(alpha = 0.5, colour = "#333333",
               fill = "#333333") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    labs(caption = title_i) + 
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
                       data = dplyr::filter(df, year == 2022)),
    "2023" = lm_robust(formula = as.formula(formula_i), 
                        data = dplyr::filter(df, year == 2023))
    )
  return(model_output)
}


#' @title Create a model summary
#' @param model_input List : list of the result of lm_robust()
create_model_summary <- function (model_input, title_n, type_output) {
  
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
                                        output = type_output)

  results_model <- model_based

  return(results_model)
  
}



create_kable_lm <- function(df, is_save = FALSE, file_name = NULL) {
  output_kable <- df |>
    mutate(
      term = case_when(
        "statistic" == "estimate" ~ "beta",
        "statistic" == "std.error" ~ "beta_se",
        TRUE ~ term
      )
    ) |> 
    select(-part, -statistic) |>
    kable(
      # format = "latex",
      escape = FALSE,
      # col.names = c("Term", "2014", "2015", "2016", "2017", "2018", "2019")
    ) |>
    kable_styling(
      full_width = FALSE,
      position = "left"
    )

  if(is_save == TRUE) {
    output_kable |>
      save_kable(
        file = here("04_output", "source", "tables", paste0(file_name, ".png")),
        self_contained = TRUE,
        zoom = 5
      )
  }
}


#' @title change countrys' name to adjust other datasets
#' @param df_country Dataframe : country master data
#' 
#' @description Change country names of the main df 
#' to match the dataset which define the level of countries.
change_country_name <- function(df) {

  df_output <- df |>
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

        df |> 
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

}


create_nw_estimate <- function(df_nw, df_bootstrap, title_i) {

  plot_nw_jp <- ggplot() +
    geom_line(data = df_nw, aes(x = x, y = y), color = "black", linewidth = 0.8) +              # ksmooth回帰線
    geom_line(data = df_bootstrap, aes(x = x, y = y_05), color = "black", linetype = "longdash", linewidth = 0.5) +  
    geom_line(data = df_bootstrap, aes(x = x, y = y_95), color = "black", linetype = "longdash", linewidth = 0.5) +  
    theme_bw(base_family = "HiraKakuPro-W3") +
    labs(
      title = title_i,
      x = "Lagged log population",
      y = "Change rate of log population(standardized)" # Change the y-axis label as needed
    ) +
    theme(
      panel.border       = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      axis.line.y.left   = element_line(color = 'black'),
      axis.line.y.right  = element_line(color = 'black'),
      axis.text.y.right  = element_blank(),
      plot.title = element_text(size = 15),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 22),
      axis.title.y = element_text(size = 22),
      panel.grid.major.y = element_line(color = "lightgray"),
      strip.background = element_blank(),
      strip.text = element_text(size = 20),
      strip.text.x = element_text(size = 18)
      ) +
    geom_hline(yintercept = 0,
               linewidth = 0.6,
               colour = "black",
               linetype = "solid") +
    facet_wrap(~ year,
               scales = "fixed",
               ncol = 3) 


}


standize_var_y <- function(df_input, var_y) {

  var_y <- enquo(var_y)

  df_summary_mean_sd <- df_input |>
    dplyr::filter(!is.infinite(!!var_y)) |>
    summarise(
      mean = mean(!!var_y, na.rm = TRUE),
      sd = sd(!!var_y, na.rm = TRUE),
      .by = year
    )
  
  df_output <- df_input |>
    left_join(df_summary_mean_sd, by = "year") |>
    mutate(
      !!var_y := (!!var_y - mean) / sd
    )
  
}


add_int_ext_change <- function(df, lag_i = 1) {

  df_output <- df |>
    arrange(county_id, year) |>
    mutate(
      lag_total = dplyr::lag(total, n = 1),
      .by = county_id
    ) |>
    mutate(
      internal_total = dplyr::lag(total, n = 1) + add_internal - exit_internal,
      external_total = dplyr::lag(total, n = 1) + add_external - exit_external,
      .by = county_id
    ) |>
    mutate(
      lag_ln_total = dplyr::lag(log(total), n = 1),
      ln_change_rate_internal = log(internal_total) - log(lag_total),
      ln_change_rate_external = log(external_total) - log(lag_total),
    )

  if(lag_i != 1) {

    df_add_exit <- df |>
      dplyr::filter(year <= 2018) |>
      summarise(
        add = sum(add, na.rm = TRUE),
        add_internal = sum(add_internal, na.rm = TRUE),
        add_external = sum(add_external, na.rm = TRUE),
        exit = sum(exit, na.rm = TRUE),
        exit_internal = sum(exit_internal, na.rm = TRUE),
        exit_external = sum(exit_external, na.rm = TRUE),
        .by = c(county_id, county_name)
      )

    df_output <- df |>
      dplyr::filter(year == 2013) |> 
      select(county_id, county_name, total) |>
      left_join(df_add_exit, by = c("county_id", "county_name")) |>
      mutate(
        ln_total = log(total),
        internal_total = total + add_internal - exit_internal,
        external_total = total + add_external - exit_external,
        .by = county_id
      ) |> 
      mutate(
        # lag_ln_total = dplyr::lag(log(total), n = 1),
        ln_change_rate_internal = log(internal_total) - log(total),
        ln_change_rate_external = log(external_total) - log(total),
      ) 
  }

  return(df_output)
}


scatter_plot_lag6 <- function(df_input, var_x, var_y, title_i) {

  var_x <- enquo(var_x)
  var_y <- enquo(var_y)

  output_plot <- ggplot(df_input,
                       aes(x = !!var_x, y = !!var_y)) +
    geom_point(alpha = 0.5, colour = "#333333",
               fill = "#333333") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    labs(caption = title_i) + 
    theme(
      panel.border       = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      axis.line.y.left   = element_line(color = 'black'),
      axis.line.y.right  = element_line(color = 'black'),
      axis.text.y.right  = element_blank(),
      plot.title         = element_text(size = 15),
      axis.text.x        = element_text(size = 18),
      axis.text.y        = element_text(size = 18),
      axis.title.x       = element_text(size = 22),
      axis.title.y       = element_text(size = 22),
      panel.grid.major.y = element_line(color = "lightgray"),
      strip.background   = element_blank(),
      strip.text         = element_text(size = 20),
      strip.text.x       = element_text(size = 18)
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
    facet_wrap(~ migration_type,
               scales = "fixed",
               ncol = 2)
    # scale_x_continuous(breaks = c(5, 10)) +
    # scale_y_continuous(
    #   breaks = c(-2, -1, 0, 1, 2),
    #   limits = c(-2, 2)
    # )
  
  return(output_plot)
}


generate_urban_rural_pref_japan <- function() {
  list_pref_urban <- c("東京都", "神奈川県", "大阪府", "愛知県", "埼玉県", "千葉県", "兵庫県")
 
  list_pref_rural <- c(
    "秋田県", "香川県", "和歌山県", "佐賀県", "山梨県", "福井県", "徳島県",
    "高知県", "島根県", "鳥取県")

  df <- readxl::read_xlsx(here("01_data", "intermediate", "japan", "jp_city_size.xlsx")) |>
    distinct(prefecture_name) |>
    mutate(
      case_when(
        prefecture_name %in% list_pref_urban ~ "urban",
        prefecture_name %in% list_pref_rural ~ "rural",
        .default = "intermediate"
      )
    )
  
  return(df)
}


generate_urban_rural_de <- function() {

  df_de_total <- read.csv(here("01_data", "intermediate", "german", "total_master.csv"))

  df_de_area <- read_xlsx(here("01_data", "raw", "german", "area", "area.xlsx")) |>
    select(
      county_id = 1,
      area = 3
    ) |>
    mutate(across(c(area, county_id), as.numeric)) |>
    dplyr::filter(!is.na(area))

  df_de_class <- df_de_total |> 
    rename(total = population) |>
    left_join(df_de_area, by = "county_id") |> 
    mutate(density = total / area) |> 
    dplyr::filter(year == 2019) |>
    mutate(
      class = case_when(
        total >= 500000 ~ "big",
        (density >= 150) & (total >=100000)  ~ "urban",
        # density >= 100 ~ "mid",
        # density < 100 ~ "rural",
        TRUE ~ "rural"
      )
    ) 
  return(df_de_class)

}
