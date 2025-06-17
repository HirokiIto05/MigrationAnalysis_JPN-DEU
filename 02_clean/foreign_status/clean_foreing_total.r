# load functions
source(here("02_clean/foreign_status/functions_stats_foreign_residents.r"))

df_japan_by_region <- map(2013:2023, clean_foreign_total_by_region) |> 
  bind_rows() 

# Add レソト, サントメ・プリンシペ since 2014
# Separate 韓国, 朝鮮 since 2015
# Add アンドラ, 赤道ギニア since 2016
# Out モナコ since 2017

df_japan_by_country <- map(2013:2023, clean_foreign_total_by_country) |> 
  bind_rows() 

change_country_name_jpn_eng_after_2022

write_csv(df, here("01_data/intermediate/foreign_status/total_by_region.csv"))

### german data

df_german <- clean_german_foreign_data() |>
  add_german_data_to_countrycode()

write_csv(df_german, here("01_data/intermediate/german/foreign_residents.csv"))


df_prefecture_by_region <- map(2013:2020, clean_prefecture_by_region) |> 
  bind_rows() 

openxlsx::write.xlsx(df_prefecture_by_region, here("01_data/intermediate/foreign_status/prefecture_by_region.xlsx"))
# Add レソト, サントメ・プリンシペ since 2014
# Separate 韓国, 朝鮮 since 2015
# Add アンドラ, 赤道ギニア since 2016
# Out モナコ since 2017
# Add 北マケドニア, Out マケドニア since 2019
# Out 赤道ギニア since 2020
df_prefecture_by_country <- map(2013:2020, clean_prefecture_by_country) |> 
  bind_rows()

# Unbalanced Panel Data because some countries are not included in some years
# df_prefecture_by_country |> summarise(
#   n = n(), 
#   .by = year
# )

openxlsx::write.xlsx(df_prefecture_by_country, here("01_data/intermediate/foreign_status/prefecture_by_country.xlsx"))
