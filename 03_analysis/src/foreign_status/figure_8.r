main <- function() {
  
  df_master <- read.csv(here::here("01_data", "intermediate", "foreign_status", "master.csv"), fileEncoding = "cp932")

  # create plot -------------------------------------------------------------
  create_result(df_master)
  
  # Adjust parameter
  base_setting <- theme(
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    panel.grid.major = element_line(color = "lightgray"),
    panel.border       = element_blank(),
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.text.y.right  = element_blank()
  ) 
    
  
  results_lag_2015_h <- high_plots_2015 + high_plots_2019 + high_plots_2022 +
    plot_annotation(title = "高技能グループ") &
    theme_bw(base_family = "HiraKakuPro-W3") &
    base_setting &
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                color = "#3C8DAD",
                linewidth = 1.3)

  
  results_lag_2019_h <- low_plots_2015 + low_plots_2019 + low_plots_2022 +
    plot_annotation(title = "技能実習・特定技能グループ") &
    theme_bw(base_family = "HiraKakuPro-W3") &
    base_setting &
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                color = "#3C8DAD",
                linewidth = 1.3)
    
  results_lag_2022_h <- status_total_plots_2015 + status_total_plots_2019 + status_total_plots_2022 +
    plot_annotation(title = "身分系グループ") &
    plot_layout(guides = 'collect') &
    theme_bw(base_family = "HiraKakuPro-W3") &
    base_setting &
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                color = "#3C8DAD",
                linewidth = 1.3)
  
  # aggregate results
  result_aggregated <- cowplot::plot_grid(
    results_lag_2015_h,
    results_lag_2019_h,
    results_lag_2022_h,
    ncol = 1, nrow = 3) 

  
  ggsave(result_aggregated,
         filename = here::here("04_output", "figures", "figure_8.png"),
         width = 12,
         height =11)
  
  
  
}


create_result <- function(df_master) {
  
  # low_5 ####
  low_plots_2015 <<- create_scatter_5(year_n = 2015,
                                     df_master,
                                     lag_ln_low_3,
                                     ln_change_rate_low_3,
                                     title_n = "2012 - 2015",
                                     title_x = "2012年人口（ln）")
  
  low_plots_2019 <<- create_scatter_5(year_n = 2019,
                                     df_master,
                                     lag_ln_low_3,
                                     ln_change_rate_low_3,
                                     title_n = "2016 - 2019",
                                     title_x = "2016年人口（ln）")
  
  low_plots_2022 <<- create_scatter_5(year_n = 2022,
                                     df_master,
                                     lag_ln_low_2,
                                     ln_change_rate_low_2,
                                     title_n = "2020 - 2022",
                                     title_x = "2020年人口（ln）")
  
  
  # high_5 ####
  
  high_plots_2015 <<- create_scatter_5(year_n = 2015,
                                      df_master, 
                                      # dplyr::filter(prefecture_name != "長崎"),
                                      lag_ln_high_3,
                                      ln_change_rate_high_3,
                                      title_n = "2012 - 2015",
                                      title_x = "2012年人口（ln）")
  
  high_plots_2019 <<- create_scatter_5(year_n = 2019,
                                      df_master,
                                      # dplyr::filter(prefecture_name != "長崎"),
                                      lag_ln_high_3,
                                      ln_change_rate_high_3,
                                      title_n = "2016 - 2019",
                                      title_x = "2016年人口（ln）")
  
  high_plots_2022 <<- create_scatter_5(year_n = 2022,
                                      df_master,
                                      # dplyr::filter(prefecture_name != "長崎"),
                                      lag_ln_high_2,
                                      ln_change_rate_high_2,
                                      title_n = "2020 - 2022",
                                      title_x = "2020年人口（ln）")
  
  # status_5 ####
  
  status_total_plots_2015 <<- create_scatter_5(year_n = 2015,
                                              df_master, 
                                              lag_ln_status_total_3,  
                                              ln_change_rate_status_total_3,
                                              title_n = "2012 - 2015",
                                              title_x = "2012年人口（ln）")
  
  status_total_plots_2019 <<- create_scatter_5(year_n = 2019,
                                              df_master, 
                                              lag_ln_status_total_3,  
                                              ln_change_rate_status_total_3,
                                              title_n = "2016 - 2019",
                                              title_x = "2016年人口（ln）")
  
  status_total_plots_2022 <<- create_scatter_5(year_n = 2022,
                                              df_master, 
                                              lag_ln_status_total_2,  
                                              ln_change_rate_status_total_2,
                                              title_n = "2020 - 2022",
                                              title_x = "2020年人口（ln）")
  
}

# lag N years ####
create_scatter_5 <- function(year_n, df_master, 
                             var_x, var_y, 
                             title_n, title_x) {
  
  
  var_x <- rlang::enquo(var_x)
  var_y <- rlang::enquo(var_y)
  
  df_plot_based <- df_master |>
    dplyr::filter(year == year_n)

  if (title_n == "2012 - 2015"){
    output_plot <- ggplot(df_plot_based,
                         # aes(x = , y = ln_change_rate)) +
                         aes(x = !!var_x, y = !!var_y)) +
      geom_point(alpha = 0.5, colour = "#333333",
                 fill = "#333333") +
      theme_classic(base_family = "HiraKakuPro-W3") +
      labs(title = title_n,
           x = title_x,
           y = "対数変化率（ln）") +
      geom_hline(yintercept = 0,
                 linewidth = 0.6,
                 colour = "black",
                 linetype = "solid") +
      geom_smooth(method = "lm",
                  formula = y ~ x,
                  se = FALSE,
                  color = "#3C8DAD",
                  linewidth = 1) +
      scale_x_continuous(breaks = c(6, 8, 10, 12),
                         limits = c(4.5, 12.5)) +
      scale_y_continuous(breaks = c(0, 0.5, 1, 1.5),
                         limits = c(-0.13, 1.52))
    
  } else {
    output_plot <- ggplot(df_plot_based,
                         # aes(x = , y = ln_change_rate)) +
                         aes(x = !!var_x, y = !!var_y)) +
      geom_point(alpha = 0.5, colour = "#333333",
                 fill = "#333333") +
      theme_classic(base_family = "HiraKakuPro-W3") +
      labs(title = title_n,
           x = title_x,
           y = " ") +
      geom_hline(yintercept = 0,
                 linewidth = 0.6,
                 colour = "black",
                 linetype = "solid") +
      geom_smooth(method = "lm",
                  formula = y ~ x,
                  se = FALSE,
                  color = "#3C8DAD",
                  linewidth = 1) +
      scale_x_continuous(breaks = c(6, 8, 10, 12),
                         limits = c(4.5, 12.5)) +
      scale_y_continuous(breaks = c(0, 0.5, 1, 1.5),
                         limits = c(-0.13, 1.52))
  }
  
  return(output_plot)  
}

main()