create_scatter <-  function(df, var_x, var_y, title_i){

  var_y = enquo(var_y)
  var_x = enquo(var_x)

  # df <- df |>
  #   mutate(
  #     !!var_y := DescTools::Winsorize(!!var_y, quantile(!!var_y, probs = c(0.01, 0.99), na.rm = TRUE))
  #   )

  df <- df |>
    filter(year > 2013) 

  num_outcome <- df |>
    pull(!!var_y) |>
    quantile(probs = c(0.01, 0.99), na.rm = TRUE)

  df <- df |>
    filter(
      between(!!var_y, num_outcome[["1%"]], num_outcome[["99%"]])
    )

  output_plot <- ggplot(df,
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
      title = title_i,
      x = "対数人口",
      y = "対数変化率（ln）") +
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
    scale_x_continuous(
      breaks = scales::pretty_breaks(n = 3)) 
    # scale_y_continuous(
    #   breaks = c(-2, -1, 0, 1, 2),
    #   limits = c(-2, 2)
    # )
  
  return(output_plot)
}