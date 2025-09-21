library(sf)

# German Data #### 
pop_grid_raw <- st_read("01_data/raw/JRC_GRID_2018/JRC_POPULATION_2018.shp")

pop_grid <- pop_grid_raw |>
  dplyr::filter(CNTR_ID == "DE")

boundary_nuts3_raw <- st_read("01_data/raw/NUTS_RG_20M_2021_3035.gpkg")

boundary_nuts3 <- boundary_nuts3_raw |>
  dplyr::filter(CNTR_CODE == "DE") |>
  dplyr::filter(LEVL_CODE == 3)

pop_grid <- st_transform(pop_grid,  st_crs(boundary_nuts3))

# --- ジオメトリの妥当化（エラー回避に有効） ---
pop_grid  <- st_make_valid(pop_grid)
boundary_nuts3 <- st_make_valid(boundary_nuts3)

# --- セルの代表点（中心点）を作る ---
# 境界上でNAになるのを避けたい場合、中心点よりも "st_point_on_surface()" が安全
pop_grid_center <- pop_grid |> 
  mutate(center_point = st_point_on_surface(geometry))

# --- 空間結合 方法1: 中心点ベース ---
# 中心点を使って結合するが、元のPOLYGON geometryは保持
pop_grid_with_centers <- pop_grid |> 
  mutate(center_point = st_point_on_surface(geometry))

# 中心点だけを抽出して結合
centers_only <- pop_grid_with_centers |>
  st_set_geometry("center_point")

centers_joined <- st_join(centers_only, boundary_nuts3, join = st_within, left = TRUE)

# 元のPOLYGONに結合結果をマージ
grid_with_nuts <- pop_grid |>
  bind_cols(
    centers_joined |> 
    st_drop_geometry() |> 
    select(starts_with("NUTS"), starts_with("CNTR"))
  )


# 結果例：CellID と NUTS3コードの対応表
out_map_geo <- grid_with_nuts |>
  select(
    year = Date,
    total_population = TOT_P_2018,
    nuts3_id = NUTS_ID,
    # county_name = NUTS_NAME,
    grid_id = GRD_ID,
    geometry
  )



df_cores_nuts3_county <- read.xlsx(here("01_data/intermediate/german/nuts_correspondence.xlsx")) |>  
  select(
    nuts3_id = nuts_code3,
    nuts2_id = nuts_code2,
    county_id,
    county_name,
    nuts2_name = city_name
  )


df_master_de <- out_map |> 
  left_join(df_cores_nuts3_county, by = c("nuts3_id")) |>
  select(
    county_id,
    county_name,
    nuts3_id,
    nuts2_id,
    nuts2_name,
    total_population,
    grid_id
  )

df_master_de_geo <- out_map_geo |> 
  left_join(df_cores_nuts3_county, by = c("nuts3_id")) |>
  select(
    county_id,
    county_name,
    nuts3_id,
    nuts2_id,
    nuts2_name,
    total_population,
    grid_id
  )


openxlsx::write.xlsx(df_master_de, "01_data/intermediate/grid_pop/germany_mesh.xlsx", rowNames = FALSE)
write_sf(df_master_de_geo, "01_data/intermediate/grid_pop/germany_mesh.gpkg", row.names = FALSE)  # コメントアウト解除

# geometry typeを確認
cat("最終的なGeometry type:", unique(st_geometry_type(df_master_de_geo)), "\n")
