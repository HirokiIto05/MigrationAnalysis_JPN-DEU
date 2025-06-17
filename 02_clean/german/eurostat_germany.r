# EUROSTAT 
# Detailed datasets -> Education and training outcomes -> 
# Educational attainment level -> 
# Population by educational attainment level, sex, age and labour status (1 000)

# 1000
df_raw <- readxl::read_xlsx(
  here(
    "01_data",
    "raw",
    "german",
    "eurostat",
    "educational_attainment_germany.xlsx"),
    sheet = 3) 

df_educ_germany <- df_raw |>
  select(
    year = 2,
    educat_1 = 3, # Less than primary, primary and lower secondary education (levels 0-2)
    educat_2 = 5, # Upper secondary and post-secondary non-tertiary education (levels 3 and 4)
    educat_3 = 7, # Tertiary education (levels 5-8)
    noresponse = 9
  ) |>
  mutate(
    across(everything(), as.numeric)) |>
  dplyr::filter(!is.na(year))


write.csv(
  df_educ_germany,
  here("01_data", "intermediate", "german", "educ_germany.csv"), 
  row.names = FALSE)


read.csv(here("01_data", "intermediate", "german", "educ_germany.csv")) |> View()
