main <- function(){
  
  # load data ####  
  df_for <- read.csv(here::here("01_data", "intermediate", "population", "overseas_master.csv"), fileEncoding = "cp932") |> 
    dplyr::filter(!year %in% c(2013, 2014)) |> 
    dplyr::mutate(ln_lag_total = log(lag_total), .after = lag_total) |> 
    dplyr::filter(social_rate > -50 & social_rate < 50)
  
  year_list <- seq(2015, 2022)
  
  
  # create_plots ####
  list_plot_for <- create_scatter_plot(df_for,
                                       var_x = ln_lag_total,
                                       var_y = social_rate,
                                       title_x = "外国人 人口(ln)")

  ggsave(list_plot_for, filename = here::here("04_output", "figures", "output_dp", "figure_6.png"),
         height = 10, width = 12)
  

}

create_scatter_plot <-  function(input_df, 
                                 var_x, var_y,
                                 title_x){
  
  var_x <- rlang::enquo(var_x)
  var_y <- rlang::enquo(var_y)
  
  list_color_1 <- c("#333333", "#A1CCD1", "#E9B384")
  list_color_2 <- c("#333333", "#3C8DAD", "#F5A962")
  

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
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        panel.grid.major.y = element_line(color = "lightgray"),
        strip.background = element_blank(),
        strip.text = element_text(size = 22),
        strip.text.x = element_text(size = 20)
      ) +
      labs(
        x = title_x,
        y = "社会増減率") +
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
                 scales = "free") +
      scale_x_continuous(breaks = c(5, 10)) +
      scale_y_continuous(limits = c(-50, 50))
  
  return(output_plot)
}

main()
