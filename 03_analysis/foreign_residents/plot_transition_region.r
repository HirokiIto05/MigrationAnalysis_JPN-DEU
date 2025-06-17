df_region <- read_csv(here("01_data/intermediate/foreign_status/total_by_region.csv"), locale = locale(encoding = "cp932"))

df_prefecture_raw <- read_csv(here("01_data/intermediate/foreign_status/prefecture_by_status.csv"), locale = locale(encoding = "cp932"))

df_prefecture <- df_prefecture_raw |>
  select(
    prefecture_name, 
    year, 
    total_foreign,
    high_skill,
    low_skill,
    training
    ) |>
  mutate(
    dummy_urban = case_when(
      prefecture_name %in% c("東京", "神奈川", "埼玉", "千葉", "大阪", "愛知") ~ "urban_area",
      TRUE ~ "not_urban_area"
    )
  ) |>
  filter(year <= 2021)

df_plot_prefecture <- df_prefecture |> 
  summarise(
    # total = sum(total_foreign),
    high_skill = sum(high_skill),
    # low_skill = sum(low_skill),
    training = sum(training),
    .by = c(dummy_urban, year)
  ) |>
  pivot_longer(
    cols = -c(dummy_urban, year),
    names_to = "category",
    values_to = "value"
  ) 

df_prefecture |> 
  summarise(
    total = sum(total_foreign),
    .by = c(year, dummy_urban)
  ) |>
  arrange(dummy_urban, year) |>
  mutate(
    change_rate = total / lag(total),
    .by = dummy_urban
  ) |>
  filter(between(year, 2013, 2018)) |>
  # ggplot(aes(x = year, y = total, color = dummy_urban)) +
  ggplot(aes(x = year, y = change_rate, color = dummy_urban)) +
  geom_point() +
  geom_line() 




# ggplot(df_plot_prefecture, aes(x = year, y = value, color = category)) +
ggplot(df_plot_prefecture, aes(x = year, y = value, color = category)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(2010, 2021, 1)
  ) +
  theme(
    axis.text.x = element_text(angle = 75, hjust = 1)
  )

df_plot_region <- df_region |>
  summarise(
    
  )

ggplot(df_plot_prefecture, aes(x = year, y = value, color = category)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(2010, 2021, 1)
  ) +
  theme(
    axis.text.x = element_text(angle = 75, hjust = 1)
  )



df_plot_region <- df_region |>
  filter(region != "Unknown") |>
  arrange(region, year) |> 
  mutate(
    change_rate = population / lag(population),
    .by = region
  ) |> 
  filter(between(year, 2013, 2018))


# ggplot(df_plot_region, aes(x = year, y = change_rate, color = region)) +
ggplot(df_plot_region, aes(x = year, y = population, color = region)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(2013, 2023, 1)
  )


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

