source(here("02_clean", "german", "residence_permit", "funs_residence_permit.r"))

list_years <- c("2013_2014", "2015_2016", "2017_2018", "2019")

# Read and aggregate permission data by years
df_res_pmt <- purrr::map(list_years, transform_df) |>
  bind_rows() |>
  ungroup()

# Read converter data
df_pmtcov <- read_permission_title_converter()

# Merge two data
df_res_pmt_final <- df_res_pmt |>
  mutate(
    total = rowSums(df_res_pmt |> select(-c(date, county_code, city_name, permission_title)), na.rm = TRUE)
  ) |>
  dplyr::filter(total != 0) |>
  mutate(
    date = lubridate::year(date)
  ) |>
  rename(year = date) |>
  left_join(df_pmtcov, by = "permission_title") 

# Check NA permission title
# the number of na is 95 
df_res_pmt_final |>
  dplyr::filter(is.na(permission_title_jpn)) |>
  distinct(sum(total))

# Check difference of rate of permission title
df_permission_jpn <- read.csv(
  here::here("01_data", "intermediate", "foreign_status", "foreign_status_rawname.csv"), 
  fileEncoding = "cp932") |>
  dplyr::filter(permission_title != "Total") |>
  summarise(
    n = sum(value, na.rm = TRUE),
    .by = c(permission_title, permission_title_jpn)
  ) |>
  mutate(
    japan = round(n / sum(n), digits = 2)
  ) |>
  select(permission_title_eng = permission_title, permission_title_jpn, japan)

df_permit_eng_jpn <- df_permission_jpn |>
  distinct(permission_title_eng, permission_title_jpn)

df_res_pmt_final <- df_res_pmt_final |>
  left_join(df_permit_eng_jpn)

# openxlsx::write.xlsx(df_res_pmt_final, here::here("01_data", "intermediate", "german", "foreign_status_rawname.xlsx"))

df_percent_germany <- df_res_pmt_final |> 
  summarise(
    n = sum(total, na.rm = TRUE),
    .by = permission_title_jpn,
  ) |>
  mutate(
    germany = round(n / sum(n), digits = 2)
  ) |>
  select(permission_title_jpn, germany)

df_percent_germany <- df_percent_germany |>
  left_join(df_permit_eng_jpn)

df_merge_pmt <- df_percent_germany |>
  full_join(df_permission_jpn) 

df_plot <- df_merge_pmt |>
  pivot_longer(
    cols = -permission_title_jpn,
    names_to = "country",
    values_to = "percent"
  )


df_merge_pmt |>
  gt()
ggplot(df_plot) +
  geom_bar(
    aes(
      x = permission_title_jpn,
      y = percent,
      color = country,
      fill = country), 
    stat = "identity",
    position = "dodge") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 75, hjust = 0.9)
  )


## check summary percent


df_german_status_sum  <- df_german_rawname |>
  mutate(
    permission_title_jpn = if_else(
      permission_title %in% c(
      "S.18(3),RA,res.perm. for empl.not requir.voc.qual.",
      "Temp. residence title according to Foreigners Act",
      "Residence title for spec. purp. acc. to For. Act",
      "S.17b(1)RA,res.perm.,training interns, EU studies"),
      "技能実習１号",
      permission_title_jpn
    )
  ) |>
  mutate(
    permission_title_eng = replace_na(permission_title_eng, "not_exist")
  ) |>
  select(year, permission_title, permission_title_eng, county_code, city_name, total) |>
  pivot_wider(
    id_cols = c(year, county_code, city_name, permission_title),
    names_from = permission_title_eng,
    values_from = total
  ) |>
  streamline_status_germany() |>
  rowwise() |>
  mutate(
    total = sum(c(high_skill, low_skill, training, status, other, not_exist), na.rm = TRUE)
  )

df_jpn_status_sum <- read.csv(
  here::here("01_data", "intermediate", "foreign_status", "foreign_status.csv"), 
  fileEncoding = "cp932") |> 
  dplyr::filter(
    between(year, 2013, 2019)) |> 
  mutate(
    japan = round(n / sum(n), digits = 2)
  ) |>
  select(permission_title_eng = permission_title, permission_title_jpn, japan)



# read.xlsx(here::here("01_data", "intermediate", "german", "foreign_status_rawname.xlsx"))
