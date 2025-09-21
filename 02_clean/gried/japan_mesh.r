main <- function() {

  library(estatapi)
  library(dplyr)

  appId <- "12604ebf628834b46b3867721893644141fe9a34"

  # One-time
  # fetch_and_save_pop_data()

  list_grid_pop_data <- list.files(here("01_data/raw/mapping/grid_pop_2015/"), full.names = TRUE)

  # Aggregate all data
  df_grid_pop <- purrr::map_dfr(list_grid_pop_data, aggregate_grid_pop_data)

  # Summarise 5次メッシュ to 3次メッシュ
  df_grid_pop <- summarise_5mesh_to_3mesh(df_grid_pop)

  # Correspondence table: Mesh to Municipalities
  df_cores_muni_mesh <- correspondence_table_mesh_to_munis()

  # Adjust for ordinance-designated cities
  df_special_area <- read_ordinance_designated_city() |>
    dplyr::rename(county_name_adjust = county_name) |>
    dplyr::select(-county_name_small)

  df_cores_muni_mesh <- adjust_ordinance_designated_city(df_cores_muni_mesh, df_special_area) |>
    mutate(mesh_id = as.character(mesh_id)) |>
    distinct(mesh_id, .keep_all = TRUE)

  df_master_jp <- df_grid_pop |> 
    left_join(df_cores_muni_mesh, by = c("area_code" = "mesh_id"), relationship = "one-to-one")

  df_shp <- aggregate_shp() |>
    select(area_code = KEY_CODE, geometry)

  df_master_jp_add_geometry <- df_master_jp |>
    left_join(df_shp, by = c("area_code"))

  df_pref_munis <- readxl::read_excel(
    here("01_data/intermediate/japan/jp_city_size.xlsx")
  ) |>
    dplyr::select(county_id, prefecture_name) |>
    dplyr::distinct()

  df_master_jp_add_geometry <- df_master_jp_add_geometry |>
    left_join(df_pref_munis, by = "county_id")


  # write.csv(df_master_jp, here("01_data/intermediate/grid_pop/japan_mesh.csv"), row.names = FALSE, fileEncoding = "cp932")
  openxlsx::write.xlsx(df_master_jp, here("01_data/intermediate/grid_pop/japan_mesh.xlsx"), rowNames = FALSE)
  st_write(df_master_jp_add_geometry, here("01_data/intermediate/grid_pop/japan_mesh.gpkg"), delete_dsn = TRUE)

}


check_meta_data <- function() {
  # 1) 統計表IDを検索：国勢調査(00200521) × 2015年 × 小地域/地域メッシュ(searchKind=2)
  lst <- estat_getStatsList(
    appId        = appId,
    statsCode    = "00200521",   # 国勢調査
    surveyYears  = "2015",       # 平成27年
    searchKind   = 2,            # 小地域・地域メッシュ
    searchWord   = "メッシュ"    # 任意。3次/1km などで絞るのも可
  )
}


fetch_and_save_pop_data <- function() {

  list_stats_id <- lst |>
    dplyr::filter(str_detect(TITLE, "人口等基本集計に関する事項 1次メッシュ")) |>  
    distinct(`@id`) |>
    pull()

  # 4) データ取得（全件）※必要に応じてcdCat01, cdTime等で絞り込み
  save_grid_pop_data <- function(stat_id, addId) {
    Sys.sleep(0.1)

    df <- estat_getStatsData(
      appId       = appId,
      statsDataId = stat_id,
      cdCat01     = "0010",
      .fetch_all  = TRUE           # 10万件超でも自動で継ぎ足し
    )

    file_name_id <- df |>
      select(starts_with("M")) |>
      colnames()

    write.csv(df, here(paste0("01_data/raw/mapping/grid_pop_2015/mesh2015_census_", file_name_id, ".csv")), row.names = FALSE, fileEncoding = "cp932")
  }

  purrr::map(list_stats_id, save_grid_pop_data, appId)
}


aggregate_grid_pop_data <- function(file_name_i) {

  df_raw <- read.csv(file_name_i, fileEncoding = "cp932")
  # df_raw <- read.csv(list_grid_pop_data[1], fileEncoding = "cp932")

  file_name_id <- df_raw |>
    select(starts_with("M")) |>
    colnames()
  
  df <- df_raw |>
    mutate(file_name = file_name_id) |>
    select(-c(unit, annotation, cat01_code, cat02_code), -starts_with("秘匿"), -starts_with("M"), -starts_with("年齢別"))

  return(df)
}


summarise_5mesh_to_3mesh <- function(df_grid_pop) {

  df_output <- df_grid_pop |>
    mutate(area_code = str_sub(area_code, 1, 8)) |> # 5次メッシュから3次メッシュに変換
    summarise(value = sum(value, na.rm = TRUE), .by = c("area_code", "file_name"))

  return(df_output)
}



correspondence_table_mesh_to_munis <- function() {

  list_cores_file <- list.files("/Users/ito_hiroki/01.Research/Railway-project/01_data/raw/mesh", full.names = TRUE)
  # メッシュコードと市区町村コードの対応表
  df_raw <- read.csv(list_cores_file[1], fileEncoding = "cp932")
 
  df_output <- purrr::map_dfr(list_cores_file, \(file_name_i) {
    df_raw <- read.csv(file_name_i, fileEncoding = "cp932")
    df <- df_raw |>
      select(county_id = "都道府県市区町村コード", county_name = "市区町村名", mesh_id = "基準メッシュ.コード")

    return(df)
  })

  return(df_output)
}


read_ordinance_designated_city <- function() {

    df_ordinance_designated_area <- dplyr::tribble(
        ~county_id, ~county_name,
        01100, "札幌市",
        01101, "札幌市 中央区",
        01102, "札幌市 北区",
        01103, "札幌市 東区",
        01104, "札幌市 白石区",
        01105, "札幌市 豊平区",
        01106, "札幌市 南区",
        01107, "札幌市 西区",
        01108, "札幌市 厚別区",
        01109, "札幌市 手稲区",
        01110, "札幌市 清田区",
        27100, "大阪市",
        27102, "大阪市 都島区",
        27103, "大阪市 福島区",
        27104, "大阪市 此花区",
        27106, "大阪市 西区",
        27107, "大阪市 港区",
        27108, "大阪市 大正区",
        27109, "大阪市 天王寺区",
        27111, "大阪市 浪速区",
        27113, "大阪市 西淀川区",
        27114, "大阪市 東淀川区",
        27115, "大阪市 東成区",
        27116, "大阪市 生野区",
        27117, "大阪市 旭区",
        27118, "大阪市 城東区",
        27119, "大阪市 阿倍野区",
        27120, "大阪市 住吉区",
        27121, "大阪市 東住吉区",
        27122, "大阪市 西成区",
        27123, "大阪市 淀川区",
        27124, "大阪市 鶴見区",
        27125, "大阪市 住之江区",
        27126, "大阪市 平野区",
        27127, "大阪市 北区",
        27128, "大阪市 中央区",
        27140, "堺市",
        27141, "堺市 堺区",
        27142, "堺市 中区",
        27143, "堺市 東区",
        27144, "堺市 西区",
        27145, "堺市 南区",
        27146, "堺市 北区",
        27147, "堺市 美原区",
        23100, "名古屋市",
        23101, "名古屋市 千種区",
        23102, "名古屋市 東区",
        23103, "名古屋市 北区",
        23104, "名古屋市 西区",
        23105, "名古屋市 中村区",
        23106, "名古屋市 中区",
        23107, "名古屋市 昭和区",
        23108, "名古屋市 瑞穂区",
        23109, "名古屋市 熱田区",
        23110, "名古屋市 中川区",
        23111, "名古屋市 港区",
        23112, "名古屋市 南区",
        23113, "名古屋市 守山区",
        23114, "名古屋市 緑区",
        23115, "名古屋市 名東区",
        23116, "名古屋市 天白区",
        26100, "京都市",
        26101, "京都市 北区",
        26102, "京都市 上京区",
        26103, "京都市 左京区",
        26104, "京都市 中京区",
        26105, "京都市 東山区",
        26106, "京都市 下京区",
        26107, "京都市 南区",
        26108, "京都市 右京区",
        26109, "京都市 伏見区",
        26110, "京都市 山科区",
        26111, "京都市 西京区",
        14100, "横浜市",
        14101, "横浜市 鶴見区",
        14102, "横浜市 神奈川区",
        14103, "横浜市 西区",
        14104, "横浜市 中区",
        14105, "横浜市 南区",
        14106, "横浜市 保土ケ谷区",
        14107, "横浜市 磯子区",
        14108, "横浜市 金沢区",
        14109, "横浜市 港北区",
        14110, "横浜市 戸塚区",
        14111, "横浜市 港南区",
        14112, "横浜市 旭区",
        14113, "横浜市 緑区",
        14114, "横浜市 瀬谷区",
        14115, "横浜市 栄区",
        14116, "横浜市 泉区",
        14117, "横浜市 青葉区",
        14118, "横浜市 都筑区",
        14130, "川崎市",
        14131, "川崎市 川崎区",
        14132, "川崎市 幸区",
        14133, "川崎市 中原区",
        14134, "川崎市 高津区",
        14135, "川崎市 多摩区",
        14136, "川崎市 宮前区",
        14137, "川崎市 麻生区",
        14150, "相模原市",
        14151, "相模原市 緑区",
        14152, "相模原市 中央区",
        14153, "相模原市 南区",
        28100, "神戸市",
        28101, "神戸市 東灘区",
        28102, "神戸市 灘区",
        28105, "神戸市 兵庫区",
        28106, "神戸市 長田区",
        28107, "神戸市 須磨区",
        28108, "神戸市 垂水区",
        28109, "神戸市 北区",
        28110, "神戸市 中央区",
        28111, "神戸市 西区",
        40100, "北九州市",
        40101, "北九州市 門司区",
        40103, "北九州市 若松区",
        40105, "北九州市 戸畑区",
        40106, "北九州市 小倉北区",
        40107, "北九州市 小倉南区",
        40108, "北九州市 八幡東区",
        40109, "北九州市 八幡西区",
        40130, "福岡市",
        40131, "福岡市 東区",
        40132, "福岡市 博多区",
        40133, "福岡市 中央区",
        40134, "福岡市 南区",
        40135, "福岡市 西区",
        40136, "福岡市 城南区",
        40137, "福岡市 早良区",
        34100, "広島市",
        34101, "広島市 中区",
        34102, "広島市 東区",
        34103, "広島市 南区",
        34104, "広島市 西区",
        34105, "広島市 安佐南区",
        34106, "広島市 安佐北区",
        34107, "広島市 安芸区",
        34108, "広島市 佐伯区",
        04100, "仙台市",
        04101, "仙台市 青葉区",
        04102, "仙台市 宮城野区",
        04103, "仙台市 若林区",
        04104, "仙台市 太白区",
        04105, "仙台市 泉区",
        12100, "千葉市",
        12101, "千葉市 中央区",
        12102, "千葉市 花見川区",
        12103, "千葉市 稲毛区",
        12104, "千葉市 若葉区",
        12105, "千葉市 緑区",
        12106, "千葉市 美浜区",
        11100, "さいたま市",
        11101, "さいたま市 西区",
        11102, "さいたま市 北区",
        11103, "さいたま市 大宮区",
        11104, "さいたま市 見沼区",
        11105, "さいたま市 中央区",
        11106, "さいたま市 桜区",
        11107, "さいたま市 浦和区",
        11108, "さいたま市 南区",
        11109, "さいたま市 緑区",
        11110, "さいたま市 岩槻区",
        22100, "静岡市",
        22101, "静岡市 葵区",
        22102, "静岡市 駿河区",
        22103, "静岡市 清水区",
        22130, "浜松市",
        22131, "浜松市 中区",
        22132, "浜松市 東区",
        22133, "浜松市 西区",
        22134, "浜松市 南区",
        22135, "浜松市 北区",
        22136, "浜松市 浜北区",
        22137, "浜松市 天竜区",
        15100, "新潟市",
        15101, "新潟市 北区",
        15102, "新潟市 東区",
        15103, "新潟市 中央区",
        15104, "新潟市 江南区",
        15105, "新潟市 秋葉区",
        15106, "新潟市 南区",
        15107, "新潟市 西区",
        15108, "新潟市 西蒲区",
        33100, "岡山市",
        33101, "岡山市 北区",
        33102, "岡山市 中区",
        33103, "岡山市 東区",
        33104, "岡山市 南区",
        43100, "熊本市",
        43101, "熊本市 中央区",
        43102, "熊本市 東区",
        43103, "熊本市 西区",
        43104, "熊本市 南区",
        43105, "熊本市 北区",
        ) |>
    tidyr::separate(
        col = county_name,
        into = c("county_name", "county_name_small"),
        sep = " "
    ) |> 
    dplyr::mutate(
        county_name_small = if_else(
            !is.na(county_name_small),
            paste0(county_name, county_name_small),
            county_name_small)
    ) |>
    dplyr::mutate(
        county_id_adjust = if_else(
            is.na(county_name_small),
            county_id,
            NA
        )
    ) |> 
    tidyr::fill(county_id_adjust)

    return(df_ordinance_designated_area)
}


adjust_ordinance_designated_city <- function(df_cores_muni_mesh, df_special_area) {

  df_output <- df_cores_muni_mesh |>
    left_join(df_special_area, by = c("county_id")) |>
    mutate(
      county_name = if_else(!is.na(county_name_adjust), county_name_adjust, county_name),
      county_id = if_else(!is.na(county_id_adjust), county_id_adjust, county_id)
    ) |>
    select(-county_name_adjust, -county_id_adjust)

  return(df_output)
}


aggregate_shp <- function() {

  list_shp_files <- list.files(here("01_data/raw/mapping/japan_grid_data/"), full.names = TRUE)

  df_shp <- purrr::map_dfr(list_shp_files, \(file_name_i) {
    shp <- sf::st_read(file_name_i, quiet = TRUE)
    return(shp)
  })

  return(df_shp)
}

