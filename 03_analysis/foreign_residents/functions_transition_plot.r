
plot_cumulative_bar <- function(df_plot){

  plot_output <- ggplot(df_plot, mapping = aes(x = year,
                                              y = population,
                                              fill = region)) +
    geom_bar(
      position="stack", 
      stat="identity") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(size = 15),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 17),
      panel.grid.major.y = element_line(color = "lightgray"),
      panel.grid.major.x = element_blank(),
      panel.border       = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      axis.line.y.left   = element_line(color = 'black'),
      axis.text.y.right  = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 15)
    ) +
    scale_fill_manual(
      name = element_blank(),
      values=c("blue", "#6096B4", "#93BFCF", "#BDCDD6", "gray", "#EEE9DA")
      # labels = c(
      #   "高技能",
      #   "技能実習・特定技能",
      #   "身分系",
      #   "特別永住者",
      #   "その他")
    ) 

  plot_output
    labs(
      y = "グループ別人口"
    ) +
    scale_y_continuous(breaks = c(0,1000000, 2000000, 3000000),
                      labels = c("0", "100万人", "200万人", "300万人")) +
    guides(fill=guide_legend(nrow = 1, byrow = TRUE))

}

