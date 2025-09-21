library(sf)
library(here)
library(dplyr)
library(igraph)
library(Matrix)  # sparseMatrix用

geo_jp_raw <- st_read(here("01_data/intermediate/grid_pop/japan_mesh.gpkg"))
geo_de_raw <- st_read(here("01_data/intermediate/grid_pop/germany_mesh.gpkg"))

geo_jp <- geo_jp_raw %>%
  mutate(density = value) 

geo_de <- geo_de_raw %>%
  rename(value = total_population) %>%
  mutate(density = value)

# 判定関数

# シンプルな分類関数
classify_grids_simple <- function(
  geo,
  dens_centre,
  dens_cluster,
  pop_centre,
  pop_cluster) {

  
  # Step 1: 密度による初期分類
  geo <- geo %>%
    mutate(
      category = case_when(
        density >= dens_centre ~ "Centre",
        density >= dens_cluster ~ "Cluster",
        TRUE ~ "Rural"
      )
    )

  # Step 2: 各セルの周辺人口を計算（自分含む）
  # 隣接判定のために全組み合わせを計算
  adj_matrix <- st_relate(geo, geo, pattern = "F***T****") # 隣接セル（タッチしているもの）

  # 各セルごとに同じカテゴリの隣接セルの人口合計を計算
  geo <- geo %>%
    mutate(
      cluster_pop = purrr::map2_dbl(
        seq_len(n()),
        category,
        ~{
          idx <- adj_matrix[[.x]]
          idx <- c(.x, idx) # 自分自身も含める
          if (.y == "Cluster") {
            # Clusterの場合はClusterとCentre両方を合計
            same_category_idx <- idx[category[idx] %in% c("Cluster", "Centre")]
          } else {
            # それ以外は同じカテゴリのみ
            same_category_idx <- idx[category[idx] == .y]
          }
          total_pop <- sum(value[same_category_idx], na.rm = TRUE)
          return(total_pop)
        }
      )
    )

  # Step 3: 人口基準で格下げ
  geo <- geo %>%
    mutate(
      category = case_when(
        # Centreの格下げ
        category == "Centre" & cluster_pop < pop_centre & density >= dens_cluster ~ "Cluster",
        category == "Centre" & cluster_pop < pop_centre & density < dens_cluster ~ "Rural",
        category == "Cluster" & cluster_pop < pop_cluster ~ "Rural",
        # Centreへの格上げ: 周辺人口がpop_centre以上ならCentre
        category != "Centre" & cluster_pop >= pop_centre ~ "Centre",
        TRUE ~ category
      ),
      category = factor(category, levels = c("Centre", "Cluster", "Rural"))
    )
  
  return(geo) 
}


geo_jp_after <- classify_grids_simple(
  geo_jp,
  dens_centre = 1500,
  dens_cluster = 300,
  pop_centre = 50000,
  pop_cluster = 10000
)

# ドイツと日本の総人口を比較し、ドイツの人口を日本に合わせてスケーリング
jp_total_pop <- sum(geo_jp$value, na.rm = TRUE)
de_total_pop <- sum(geo_de$value, na.rm = TRUE)
de_pop_scale <- de_total_pop / jp_total_pop

geo_de_after <- classify_grids_simple(
  geo_de,
  dens_centre = 1500 * de_pop_scale,
  dens_cluster = 300,
  pop_centre = 50000 * de_pop_scale,
  pop_cluster = 10000
)

# 市区町村レベルでの分類関数
geo_classified <- geo_de_after 
classify_counties <- function(geo_classified) {
  county_classification <- geo_classified %>%
    st_drop_geometry() %>%
    group_by(county_id) %>% 
    summarise(
      total_pop = sum(value, na.rm = TRUE),
      centre_pop = sum(ifelse(category == "Centre", value, 0), na.rm = TRUE),
      cluster_pop = sum(ifelse(category == "Cluster", value, 0), na.rm = TRUE),
      rural_pop = sum(ifelse(category == "Rural", value, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      centre_pct = centre_pop / total_pop * 100,
      cluster_pct = cluster_pop / total_pop * 100,
      rural_pct = rural_pop / total_pop * 100,
      
      # 分類ロジック
      county_type = case_when(
        centre_pct >= 50 ~ "Cities",
        cluster_pct >= 50 ~ "Towns & semi-dense areas", 
        rural_pct >= 50 ~ "Rural areas",
        # どのカテゴリも50%に達しない場合は最も多い割合のカテゴリに分類
        centre_pct >= cluster_pct & centre_pct >= rural_pct ~ "Cities",
        cluster_pct >= centre_pct & cluster_pct >= rural_pct ~ "Towns & semi-dense areas",
        TRUE ~ "Rural areas"
      )
    ) %>%
    select(county_id, total_pop, centre_pct, cluster_pct, rural_pct, county_type, ends_with("pop"))
  
  return(county_classification)
}

st_write(geo_jp_after, here("01_data/intermediate/grid_pop/japan_grid_classified.gpkg"), delete_dsn = TRUE)
st_write(geo_de_after, here("01_data/intermediate/grid_pop/germany_grid_classified.gpkg"), delete_dsn = TRUE)


# 日本とドイツの市区町村分類
jp_county_classification <- classify_counties(geo_jp_after)
de_county_classification <- classify_counties(geo_de_after)
    

# 結果確認
cat("日本の市区町村分類:\n")
jp_county_classification %>%
  count(county_type) %>%
  mutate(percentage = round(n/sum(n)*100, 1)) %>%
  print()

cat("\nドイツの市区町村分類:\n") 
de_county_classification %>%  
  count(county_type) %>%
  mutate(percentage = round(n/sum(n)*100, 1)) %>%
  print()

# 結果をファイルに保存
jp_county_classification %>%
  openxlsx::write.xlsx(here("01_data/intermediate/grid_pop/jp_county_classification.xlsx"))

de_county_classification %>%
  openxlsx::write.xlsx(here("01_data/intermediate/grid_pop/de_county_classification.xlsx"))

# 都道府県・NUTS2レベルでの分類関数
classify_regions <- function(geo_classified, region_col) {
  region_classification <- geo_classified %>%
    st_drop_geometry() %>%
    group_by(!!sym(region_col)) %>%
    summarise(
      total_pop = sum(value, na.rm = TRUE),
      centre_pop = sum(ifelse(category == "Centre", value, 0), na.rm = TRUE),
      cluster_pop = sum(ifelse(category == "Cluster", value, 0), na.rm = TRUE),
      rural_pop = sum(ifelse(category == "Rural", value, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      centre_pct = centre_pop / total_pop * 100,
      cluster_pct = cluster_pop / total_pop * 100,
      rural_pct = rural_pop / total_pop * 100,
      
      # 分類ロジック
      region_type = case_when(
        centre_pct >= 50 ~ "Cities",
        cluster_pct >= 50 ~ "Towns & semi-dense areas", 
        rural_pct >= 50 ~ "Rural areas",
        # どのカテゴリも50%に達しない場合は最も多い割合のカテゴリに分類
        centre_pct >= cluster_pct & centre_pct >= rural_pct ~ "Cities",
        cluster_pct >= centre_pct & cluster_pct >= rural_pct ~ "Towns & semi-dense areas",
        TRUE ~ "Rural areas"
      )
    ) %>%
    select(!!sym(region_col), total_pop, centre_pct, cluster_pct, rural_pct, region_type)
  
  return(region_classification)
}

# 都道府県レベル分類（日本）
jp_prefecture_classification <- classify_regions(geo_jp_after, "prefecture_name")
openxlsx::write.xlsx(jp_prefecture_classification, here("01_data/intermediate/grid_pop/jp_prefecture_classification.xlsx"))

# NUTS2レベル分類（ドイツ）
de_nuts2_classification <- classify_regions(geo_de_after, "nuts2_name")
openxlsx::write.xlsx(de_nuts2_classification, here("01_data/intermediate/grid_pop/de_nuts2_classification.xlsx"))



# 市区町村と都市レベルのマッピングテーブル
jp_county_city_map <- jp_county_classification %>%
  select(county_id, county_type)

df_county_name <-  geo_de_after %>%
  st_drop_geometry() %>%
  distinct(county_id, county_name) 

de_county_city_map <- de_county_classification %>%
  select(county_id, county_type) |>
  left_join(df_county_name, by = "county_id")


# 都道府県/NUTS2と都市レベルのマッピングテーブル
jp_pref_city_map <- jp_prefecture_classification %>%
  select(prefecture_name, region_type)

df_nuts2_name <-  geo_de_after %>%
    st_drop_geometry() %>%
    distinct(nuts2_id, nuts2_name) 

de_nuts2_city_map <- de_nuts2_classification %>% 
  select(nuts2_name, region_type) |>
  left_join(df_nuts2_name, by = "nuts2_name") 

# 保存
openxlsx::write.xlsx(jp_county_city_map, here("01_data/intermediate/grid_pop/jp_county_city_map.xlsx"))
openxlsx::write.xlsx(de_county_city_map, here("01_data/intermediate/grid_pop/de_county_city_map.xlsx"))
openxlsx::write.xlsx(jp_pref_city_map, here("01_data/intermediate/grid_pop/jp_pref_city_map.xlsx"))
openxlsx::write.xlsx(de_nuts2_city_map, here("01_data/intermediate/grid_pop/de_nuts2_city_map.xlsx"))



# 結果確認
cat("日本の都道府県分類:\n")
jp_prefecture_classification %>% 
  count(region_type) %>%
  mutate(percentage = round(n/sum(n)*100, 1)) %>%
  print()

cat("\nドイツのNUTS2分類:\n") 
de_nuts2_classification %>%
  count(region_type) %>%
  mutate(percentage = round(n/sum(n)*100, 1)) %>%
  print()

# 分類表の作成
create_classification_table <- function(grid_data, county_data, region_data, country_name) {
  
  # グリッドレベル集計
  grid_summary <- grid_data %>%
    st_drop_geometry() %>%
    summarise(
      level = "Grid",
      cities = sum(category == "Centre"),
      towns = sum(category == "Cluster"),
      rural = sum(category == "Rural"),
      total = n(),
      .groups = "drop"
    )
  
  # 市区町村レベル集計
  county_summary <- county_data %>%
    summarise(
      level = "County/Municipality",
      cities = sum(county_type == "Cities"),
      towns = sum(county_type == "Towns & semi-dense areas"),
      rural = sum(county_type == "Rural areas"),
      total = n(),
      .groups = "drop"
    )
  
  # 都道府県/NUTS2レベル集計
  region_summary <- region_data %>%
    summarise(
      level = ifelse(country_name == "Japan", "Prefecture", "NUTS2"),
      cities = sum(region_type == "Cities"),
      towns = sum(region_type == "Towns & semi-dense areas"),
      rural = sum(region_type == "Rural areas"),
      total = n(),
      .groups = "drop"
    )
  
  # 統合表
  classification_table <- bind_rows(grid_summary, county_summary, region_summary) %>%
    mutate(
      country = country_name,
      cities_pct = round(cities/total*100, 1),
      towns_pct = round(towns/total*100, 1),
      rural_pct = round(rural/total*100, 1)
    ) %>%
    select(country, level, cities, cities_pct, towns, towns_pct, rural, rural_pct, total)
  
  return(classification_table)
}

# 日本とドイツの分類表作成
jp_classification_table <- create_classification_table(
  geo_jp_after, jp_county_classification, jp_prefecture_classification, "Japan"
)


de_classification_table <- create_classification_table(
  geo_de_after, de_county_classification, de_nuts2_classification, "Germany"
)

# 統合分類表
combined_classification_table <- bind_rows(jp_classification_table, de_classification_table)

# 表示
cat("\n=== 日独都市化度分類表 ===\n")
print(combined_classification_table)

# 保存
jp_prefecture_classification %>%
  openxlsx::write.xlsx(here("01_data/intermediate/grid_pop/jp_prefecture_classification.xlsx"))

de_nuts2_classification %>%
  openxlsx::write.xlsx(here("01_data/intermediate/grid_pop/de_nuts2_classification.xlsx"))

combined_classification_table %>%
  openxlsx::write.xlsx(here("01_data/intermediate/grid_pop/combined_classification_table.xlsx"))


