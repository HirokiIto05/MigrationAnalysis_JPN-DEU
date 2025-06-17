read_data <- function(file_year) {

  df_raw <- read.csv(here(
    "01_data", 
    "raw", 
    "german", 
    "residence_permit", 
    paste0(file_year, ".csv")))
}

transform_df <- function(file_year) {
  print(file_year)

  df_raw <- read_data(file_year)
  
  df <- df_raw |>
    select(
      "date" = 1,
      "county_code" = 2,
      "city_name" = 3,
      "permission_title" = 4,
      5:ncol(df_raw)
    ) |>
    janitor::clean_names() |>
    mutate(
      across(-c(date, county_code, city_name, permission_title), as.numeric)
    )
  
  df_output <- df |>
    mutate(
      europe_new = rowSums(df |> select(starts_with("europe")), na.rm = TRUE),
      africa_new = rowSums(df |> select(starts_with("africa")), na.rm = TRUE),
      america_new = rowSums(df |> select(starts_with("america")), na.rm = TRUE),
      asia_new = rowSums(df |> select(starts_with("asia")), na.rm = TRUE),
      australia_and_oceania_new = rowSums(df |> select(starts_with("australia_and_oceania")), na.rm = TRUE)
    ) |>
    select(
      date,
      county_code,
      city_name,
      permission_title,
      europe = europe_new,
      africa = africa_new,
      america = america_new,
      asia = asia_new,
      australia_and_oceania = australia_and_oceania_new
    ) |> 
    dplyr::filter(!is.na(county_code)) |>
    ungroup()

  return(df_output)
}


read_permission_title_converter <- function() {
  df_pmtcov <- readxl::read_xlsx(here("01_data", "intermediate", "german", "permission_converter_modified.xlsx")) |>
    select(
      permission_title = residence_permit_germany,
      permission_title_jpn = short_jpn
    )
}


streamline_status_germany <- function(df) {
  
  # 区分に分けて合計を算出
  df_output <- df |> 
    dplyr::summarise(high_skill = 
                       sum(
                        Highly_Skilled_Professional_1_a,
                        Highly_Skilled_Professional_2,
                        Business_Manager,
                        Research,
                        Intra_Company_Transferee,
                        Skill, 
                        na.rm = TRUE),
                     low_skill =
                       sum(
                        Technical_Intern_Training_1_a,
                        Specified_Skilled_Worker_1,
                        na.rm = TRUE),
                     training =
                       sum(
                        Technical_Intern_Training_1_a,
                        na.rm = TRUE),
                     status =
                       sum(
                        Permanent_Resident,
                        Spouse_or_Child_of_Japanese_National,
                        Spouse_or_Child_of_Permanent_Resident,
                        Long_Term_Resident, 
                        na.rm = TRUE),
                     other = sum(
                      Student, 
                      Trainee, 
                      Dependent,
                      Designated_Activities,
                      na.rm = TRUE),
                    not_exist = sum(not_exist, na.rm = TRUE),
                    .by = c("county_code", "city_name", "year")) 

  return(df_output)
  
}



