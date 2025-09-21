# Raw data
main <- function() {

  df_raw <- read.csv(here("01_data", "raw", "country_classification", "country_classification.csv")) 

  df <- clean_country_class(df_raw)

  openxlsx::write.xlsx(
    df, 
    here("01_data", "intermediate", "german", "country_classification_master.xlsx")
  )

}

clean_country_class <- function(df_country_class) {

  df_output <- df_country_class |>
    janitor::clean_names() |> 
    select(iso_code, country_name, cls_income = x2019) |> 
    mutate(
      cls_income = case_when(
        cls_income == "H" ~ "high",
        cls_income == "UM" ~ "upper_middle",
        cls_income == "LM" ~ "lower_middle",
        cls_income == "L" ~ "low",
        TRUE ~ cls_income
      )
    ) |>
    mutate(
      country_name = case_when(
        country_name == "H" ~ "high",
        country_name == "UM" ~ "upper_middle",
        country_name == "LM" ~ "lower_middle",
        country_name == "L" ~ "low",
        TRUE ~ country_name
      )
    ) |>
    mutate(
      country_name = if_else(str_detect(country_name, "d'Ivoire"), "Cote d'Ivoire", country_name),
      country_name = if_else(str_detect(country_name, "Tom and Pr_cipe"), "Sao Tome and Principe", country_name)
      )

}
