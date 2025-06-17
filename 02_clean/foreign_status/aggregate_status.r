main <- function() {
  
  # generate constants ---------------------------------------------------------------------
  # 年ごとに在留区分が変わるため列名を分けている
  l_col_1 <- create_list_06_09()
  l_col_2 <- create_list_10_11()
  l_col_3 <- create_list_12_14()
  l_col_4 <- create_list_15_16()
  l_col_5 <- create_list_17_18()
  l_col_6 <- create_list_19_22()
  
  list_cols <- list(
    "year_06_09" = l_col_1,
    "year_10_11" = l_col_2,
    "year_12_14" = l_col_3,
    "year_15_16" = l_col_4,
    "year_17_18" = l_col_5,
    "year_19_22" = l_col_6
    )
  
  df_filenames <- dplyr::tribble(
    ~"year", ~"file_name",
    "2010", "10-99-04-0.xls",
    "2011", "11-99-04-0.xls",
    "2012", "12-12-05-0.xls",
    "2013", "13-12-05-0.xlsx",
    "2014", "14-12-05-0.xlsx",
    "2015", "15-12-05-0.xlsx",
    "2016", "16-12-05-0.xlsx",
    "2017", "17-12-05-0.xlsx",
    "2018", "18-12-05-0.xlsx",
    "2019", "19-12-05.xlsx",
    "2020", "20-12-05.xlsx",
    "2021", "21-12-03-2.xlsx",
    "2022", "22-06-03-1 .xlsx" # 2022年のみ6月
  )
  
  # 2010 - 2021(Dec), 2022(Jun)
  list_year <- rev(seq(2010, 2022))

  df_foreign <- purrr::map(list_year, create_df_foreign, list_cols, df_filenames) |> 
    dplyr::bind_rows() |> 
    dplyr::mutate_all(~stringr::str_replace_all(., "-", "0")) |> 
    dplyr::mutate_at(dplyr::vars(-prefecture_name), as.numeric) |> 
    dplyr::filter(!prefecture_name  %in% c("第５表　都道別　在留資格別　在留外国人　（総　数）",
                                           "総数",
                                           "第５表　都道府県別　在留資格別　在留外国人　（総　数）",
                                           "未定・不詳",
                                           "未定・不祥"))

  df_jpn <- create_status_jpn(df_foreign)

  df_foreign_rawname  <- df_foreign |>
    tidyr::pivot_longer(
      cols = -c(prefecture_name, year),
      names_to = "permission_title",
      values_to = "value"
    ) |>
    left_join(
      df_jpn, by = "permission_title"
    )
  write.csv(df_foreign_rawname, here::here("01_data", "intermediate", "foreign_status", "foreign_status_rawname.csv"), fileEncoding = "cp932", row.names = FALSE)


  df_foreign <- streamline_status(df_foreign)
  write.csv(df_foreign, here::here("01_data", "intermediate", "foreign_status", "foreign_status.csv"), fileEncoding = "cp932", row.names = FALSE)
}


create_df_foreign <- function(year_n, list_cols, df_filenames) {
  print(year_n)

  file_name <- df_filenames |>
    dplyr::filter(year == year_n) |>
    dplyr::pull(file_name)
  
  if(year_n <= 2010) {

    df_based <- readxl::read_xls(here::here("01_data", "raw", "foreign_residents_status", file_name),
                                 col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_10_11"]])
    
  } else if (year_n <= 2011) {

    df_based <- readxl::read_xls(here::here("01_data", "raw", "foreign_residents_status", file_name),
                                 col_names = FALSE) |> 
      dplyr::select(-1) 
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_10_11"]])
    
  } else if (year_n <= 2012) {

      df_based <- readxl::read_xls(here::here("01_data", "raw", "foreign_residents_status", file_name),
                                   col_names = FALSE) |> 
        dplyr::select(-1)
      
      colnames(df_based) <- c("prefecture_name", list_cols[["year_12_14"]])
  } else if (year_n <= 2014) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                      col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_12_14"]])
  } else if (year_n <= 2016) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                      col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_15_16"]])
    
  } else if (year_n <= 2018) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                      col_names = FALSE) |> 
      dplyr::select(-1)
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_17_18"]])
    
  } else if (year_n <= 2020) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                      col_names = FALSE) 
    
    colnames(df_based) <- c("prefecture_name", list_cols[["year_19_22"]])
    
    df_based <- df_based |> 
      dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "県", "")) |> 
      dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "府", "")) |> 
      dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "東京都", "東京"))
  } else if (year_n <= 2022) {

    df_based <- readxl::read_xlsx(here::here("01_data", "raw", "foreign_residents_status", file_name),
                                  col_names = FALSE) |> 
      dplyr::select(-1)

    colnames(df_based) <- c("prefecture_name", list_cols[["year_19_22"]])

  df_based <- df_based |> 
    dplyr::filter(stringr::str_detect(prefecture_name, "県|京都府|大阪府|東京都|北海道")) |> 
    dplyr::filter(!stringr::str_detect(prefecture_name, "都道府県市区町村|山県市")) |> 
    dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "県", "")) |> 
    dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "府", "")) |> 
    dplyr::mutate(prefecture_name = stringr::str_replace_all(prefecture_name, "東京都", "東京"))
  }
  
  df_output <- df_based |> 
    tidyr::drop_na(prefecture_name) |>
    dplyr::mutate(year = year_n, .after = prefecture_name)

  return(df_output)
}


create_list_06_09 <- function() {

  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Investor_or_Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer",
                   "Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Entertainer",
                   "Skill",
                   "Cultural_Activities",
                   "Temporary_Visitor",
                   "Student",
                   "Student_High_School",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Specific_Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Unacquired",
                   "Temporary_Protect",
                   "Other")
  
  
  return(list_output)  
  
}


create_list_10_11 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Investor_or_Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer",
                   "Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Entertainer",
                   "Skill",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Cultural_Activities",
                   "Temporary_Visitor",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Specific_Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Unacquired",
                   "Temporary_Protect",
                   "Other")
  
  
  return(list_output)  
  
}


create_list_12_14 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Investor_or_Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer",
                   "Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Entertainer",
                   "Skill",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Cultural_Activities",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Specific_Permanent_Resident")
  
  
  return(list_output)  
  
}


create_list_15_16 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Highly_Skilled_Professional_1_a",
                   "Highly_Skilled_Professional_1_b",
                   "Highly_Skilled_Professional_1_c",
                   "Highly_Skilled_Professional_2",
                   "Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer_or_Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Entertainer",
                   "Skill",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Cultural_Activities",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Specific_Permanent_Resident")
  
  
  return(list_output)  
  
}


create_list_17_18 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Highly_Skilled_Professional_1_a",
                   "Highly_Skilled_Professional_1_b",
                   "Highly_Skilled_Professional_1_c",
                   "Highly_Skilled_Professional_2",
                   "Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer_or_Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Nursing_Care",
                   "Entertainer",
                   "Skill",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Technical_Intern_Training_3_a",
                   "Technical_Intern_Training_3_b",
                   "Cultural_Activities",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Specific_Permanent_Resident")
  

  return(list_output)  
  
}


create_list_19_22 <- function() {
  
  list_output <- c("Total",
                   "Professor", 
                   "Art", 
                   "Religion",
                   "Report",
                   "Highly_Skilled_Professional_1_a",
                   "Highly_Skilled_Professional_1_b",
                   "Highly_Skilled_Professional_1_c",
                   "Highly_Skilled_Professional_2",
                   "Business_Manager",
                   "Legal_and_Accounting_Services",
                   "Medical_Services",
                   "Research",
                   "Educate",
                   "Engineer_or_Specialist_in_Humanities_or_International_Services",
                   "Intra_Company_Transferee",
                   "Nursing_Care",
                   "Entertainer",
                   "Skill",
                   "Specified_Skilled_Worker_1",
                   "Specified_Skilled_Worker_2",
                   "Technical_Intern_Training_1_a",
                   "Technical_Intern_Training_1_b",
                   "Technical_Intern_Training_2_a",
                   "Technical_Intern_Training_2_b",
                   "Technical_Intern_Training_3_a",
                   "Technical_Intern_Training_3_b",
                   "Cultural_Activities",
                   "Student",
                   "Trainee",
                   "Dependent",
                   "Designated_Activities",
                   "Permanent_Resident",
                   "Spouse_or_Child_of_Japanese_National",
                   "Spouse_or_Child_of_Permanent_Resident",
                   "Long_Term_Resident",
                   "Specific_Permanent_Resident")
  

  return(list_output)  
  
}


create_status_jpn <- function(df_foreign) {
  
  df_status_eng <- dplyr::tibble(
    permission_title = colnames(df_foreign)
    ) |> 
    dplyr::mutate(id = row_number(), .before = permission_title)
  
  list_status_jap <- c("prefecture_name",
                       "year",
                       "総数",
                       "教授",
                       "芸術",
                       "宗教",
                       "報道",
                       "高度専門職１号イ",
                       "高度専門職１号ロ",
                       "高度専門職１号ハ",
                       "高度専門職２号",
                       "経営_管理",
                       "法律_会計業務",
                       "医療",
                       "研究",
                       "教育",
                       "技術_人文知識_国際",
                       "企業内転勤",
                       "介護",
                       "興行",
                       "技能",
                       "特定技能１号",
                       "特定技能２号",
                       "技能実習１号イ",
                       "技能実習１号ロ",
                       "技能実習２号イ",
                       "技能実習２号ロ",
                       "技能実習３号イ",
                       "技能実習３号ロ",
                       "文化活動",
                       "留学",
                       "研修",
                       "家族滞在",
                       "特定活動",
                       "永住者",
                       "日本人の配偶者等",
                       "永住者の配偶者等",
                       "定住者",
                       "特別永住者",
                       "投資_経営",
                       "技術",
                       "人文知識_国際",
                       "短期滞在",
                       "未取得者",
                       "一時庇護",
                       "その他")
  
  df_status_jap <- tibble(permission_title_jpn = list_status_jap) |> 
    dplyr::mutate(id = row_number(), .before = permission_title_jpn)
  
  df_output <- df_status_eng |> 
    dplyr::left_join(df_status_jap) 
    # pivot_wider(names_from = c(eng), values_from = c(jap))
  
  return(df_output)
  
}


streamline_status <- function(df_foreign) {
  
  # 「技術」と「人文知識・国際業務」を統合
  #「経営・管理」と「投資・経営」
  df_based <- df_foreign |> 
    dplyr::group_by(prefecture_name, year) |>  
    dplyr::mutate(Engineer_or_Specialist_in_Humanities_or_International_Services = 
                    if_else(is.na(Engineer), 
                            Engineer_or_Specialist_in_Humanities_or_International_Services,
                            sum(Engineer, Specialist_in_Humanities_or_International_Services, na.rm = TRUE))) |> 
    dplyr::select(-Engineer, -Specialist_in_Humanities_or_International_Services) |> 
    dplyr::mutate(Business_Manager = 
                    if_else(is.na(Investor_or_Business_Manager), 
                            Business_Manager,
                            Investor_or_Business_Manager)) |> 
    dplyr::select(-Investor_or_Business_Manager) |>
    dplyr::ungroup()
  

  df_except <- df_based |> 
    dplyr::select(-Total)
  
  # 区分に分けて合計を算出
  df_output <- df_based |> 
    dplyr::summarise(total_foreign = 
                       Total,
                     high_skill = 
                       sum(Professor,
                           Art,
                           Religion,
                           Report,
                           Highly_Skilled_Professional_1_a,
                           Highly_Skilled_Professional_1_b,
                           Highly_Skilled_Professional_1_c,
                           Highly_Skilled_Professional_2,
                           Business_Manager,
                           Legal_and_Accounting_Services,
                           Medical_Services,
                           Research,
                           Educate,
                           Engineer_or_Specialist_in_Humanities_or_International_Services,
                           Intra_Company_Transferee,
                           Nursing_Care,
                           Skill, na.rm = TRUE),
                     low_skill =
                       sum(Specified_Skilled_Worker_1,
                           Specified_Skilled_Worker_2,
                           Technical_Intern_Training_1_a,
                           Technical_Intern_Training_1_b,
                           Technical_Intern_Training_2_a,
                           Technical_Intern_Training_2_b,
                           Technical_Intern_Training_3_a,
                           Technical_Intern_Training_3_b, na.rm = TRUE),
                     training =
                       sum(Technical_Intern_Training_1_a,
                           Technical_Intern_Training_1_b,
                           Technical_Intern_Training_2_a,
                           Technical_Intern_Training_2_b,
                           Technical_Intern_Training_3_a,
                           Technical_Intern_Training_3_b, na.rm = TRUE),
                     status =
                       sum(Permanent_Resident,
                           Spouse_or_Child_of_Japanese_National,
                           Spouse_or_Child_of_Permanent_Resident,
                           Long_Term_Resident, na.rm = TRUE),
                     specific_permanent_resident = Specific_Permanent_Resident,
                     other = sum(Unacquired, 
                                 Student, 
                                 Temporary_Protect,
                                 Entertainer,
                                 Cultural_Activities,
                                 Trainee, 
                                 Designated_Activities,
                                 Dependent,
                                 Other, 
                                 Temporary_Visitor, na.rm = TRUE),
                                 .by = c("prefecture_name", "year")) |> 
    dplyr::mutate(except_specific = total_foreign - specific_permanent_resident)

  return(df_output)
  
}


main()
