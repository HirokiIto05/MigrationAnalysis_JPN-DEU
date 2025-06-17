main <- function() {
  
  df_master <- read.csv(here::here("01_data", "intermediate", "foreign_status", "master.csv"), fileEncoding = "cp932")

  df_plot_based <- modify_dataset(df_master)

  plot_cumulative <- generate_cumulative_plot(df_plot_based)  

  ggsave(
    plot_cumulative, 
    filename = here::here("04_output", "figures", "figure_7.png"),
    width = 10, 
    height = 7)
}



modify_dataset <- function(df_master){

  df_based <- df_master |> 
    dplyr::filter(year %in% seq(2010,2022, 2)) |> 
    dplyr::summarise(total = sum(total_foreign, na.rm = TRUE),
              high_skill = sum(high_skill, na.rm = TRUE),  
              low_skill = sum(low_skill, na.rm = TRUE),  
              # training = sum(training, na.rm = TRUE),
              status = sum(status, na.rm = TRUE),
              specific_residents = sum(specific_permanent_resident, na.rm = TRUE),
              other = sum(other, na.rm = TRUE), .by = year) |>
    dplyr::mutate_at(vars(-year), round) |> 
    dplyr::mutate(total_rate = round((total/total)*100, digits = 1), .after = total) |> 
    dplyr::mutate(high_skill_rate = round((high_skill/total)*100, digits = 1), .after = high_skill) |> 
    dplyr::mutate(low_skill_rate = round((low_skill/total)*100, digits = 1), .after = low_skill) |> 
    dplyr::mutate(status_rate = round((status/total)*100, digits = 1), .after = status) |> 
    dplyr::mutate(specific_residents_rate =  round((specific_residents/total)*100, digits = 1), .after = specific_residents) |> 
    dplyr::mutate(other_rate =  round((other/total)*100, digits = 1), .after = other)


  df_plot_based <- df_based |> 
    dplyr::select(
      !ends_with("rate")
      ) |>
    tidyr::pivot_longer(
      cols = !year,
      names_to = "category",
      values_to = "value"
    ) |> 
    dplyr::filter(
      # category != "total_rate"
      category != "total"
    ) |> 
    dplyr::mutate(
      year = as.character(year),
      category = dplyr::recode_factor(category,
                                      "high_skill" = "high_skill",
                                      "low_skill" = "low_skill",
                                      "status" = "status",
                                      "specific_residents" ="specific_residents",
                                      "other" = "other")
    ) |> 
    dplyr::filter(
      year != 2010
    )


  return(df_plot_based) 

}



generate_cumulative_plot <- function(df_plot_based){

  plot_output <- ggplot(df_plot_based, mapping = aes(x = year,
                                              y = value,
                                              fill = category)) +
    geom_bar(df_plot_based, mapping = aes(x = year,
                                          y = value,
                                          fill = category),
            position="stack", stat="identity") +
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
      values=c("#6096B4", "#93BFCF", "#BDCDD6", "gray", "#EEE9DA"),
      labels = c(
        "高技能",
        "技能実習・特定技能",
        "身分系",
        "特別永住者",
        "その他")
    ) +
    labs(
      y = "グループ別人口"
    ) +
    scale_y_continuous(breaks = c(0,1000000, 2000000, 3000000),
                      labels = c("0", "100万人", "200万人", "300万人")) +
    guides(fill=guide_legend(nrow = 1, byrow = TRUE))


  return(plot_output)
}


main()