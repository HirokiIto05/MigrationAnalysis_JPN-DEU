main <- function() {
  
  df_master <- read.csv(here::here("01_data", "intermediate", "foreign_status", "master.csv"), fileEncoding = "cp932")

  stats_table_output <- create_stats_table(df_master)

  openxlsx::write.xlsx(
    stats_table_output, 
    file = here::here("04_output", "tables", "appendix_table_3.xlsx"))
}


create_stats_table <- function(df_master){

  df_table_based <- df_master |>
    dplyr::select(
      -dplyr::starts_with("lag"),
      -dplyr::starts_with("ln"),
      -status,
      -except_specific
    ) |>
    dplyr::filter(
      year %% 2 == 0,
      dplyr::between(year, 2012, 2022)
    ) |>
    dplyr::distinct() 

  stats_table_output <- df_table_based |>
    reframe(
      across(-any_of(c("id", "prefecture_name", "year")), sum),
      .by = c("year")
    ) |> 
    dplyr::mutate(
      total_foreign_rate = round((total_foreign / total_foreign)*100, digits = 1),
      high_skill_rate = round((high_skill / total_foreign)*100, digits = 1),
      low_skill_rate = round((low_skill / total_foreign)*100, digits = 1),
      status_total_rate = round(((status_total - specific_permanent_resident) / total_foreign)*100, digits = 1),
      other_rate = round((other / total_foreign)*100, digits = 1)
    ) |> 
    dplyr::select(
      year,
      dplyr::starts_with("high_skill"),
      dplyr::starts_with("low"),
      dplyr::starts_with("status"),
      dplyr::starts_with("other"),
      dplyr::starts_with("total_foreign")
    ) |>
    tidyr::pivot_longer(
      cols = -year,
      names_to = "category",
      values_to = "value"
    ) |> 
    tidyr::pivot_wider(
      id_cols = category,
      names_from = year,
      values_from = value
  )

  return(stats_table_output)

}


main()