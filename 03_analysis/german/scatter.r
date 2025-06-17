df <- readxl::read_xlsx("/Users/ito_hiroki/Downloads/20-12-07-1m.xlsx") |>
  select(
    prefecture = 在留外国人統計,
    name_muni = ...3,
    nationality = ...4,
    value = ...5
  ) |> 
  mutate(
    # value = stringr::str_repalce_all(value, ",", ""),
    value = as.numeric(value)
  ) |> 
  dplyr::filter(name_muni != "総数") |>
  pivot_wider(
    id_cols = c(name_muni, prefecture),
    names_from = nationality,
    values_from = value
  ) |>
  mutate(
    other_rate = (その他/総数) * 100,
    .after = prefecture
  )

df |> View()

df |>colnames()
