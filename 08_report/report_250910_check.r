# ドイツのgeometryをチェック
cat("=== ドイツのgeometry情報 ===\n")
cat("行数:", nrow(geo_de_raw), "\n")
cat("CRS:", st_crs(geo_de_raw)$input, "\n")
cat("Geometry type:", unique(st_geometry_type(geo_de_raw)), "\n")
cat("Geometry列名:", attr(geo_de_raw, "sf_column"), "\n")
cat("bbox:\n")
print(st_bbox(geo_de_raw))


# 最初の5つのgeometryを確認
cat("\n最初の5つのgeometry:\n")
for(i in 1:min(5, nrow(geo_de_raw))) {
  geom <- st_geometry(geo_de_raw[i,])[[1]]
  cat("Row", i, ": coords =", length(geom), "points\n")
}

# 日本との比較
cat("\n=== 日本のgeometry情報 ===\n")
cat("行数:", nrow(geo_jp_raw), "\n")
cat("CRS:", st_crs(geo_jp_raw)$input, "\n") 
cat("Geometry type:", unique(st_geometry_type(geo_jp_raw)), "\n")

# データ準備
geo_de <- geo_de_raw %>%
  rename(value = total_population) %>%
  mutate(density = value)

geo_jp <- geo_jp_raw %>% 
  mutate(density = value)

# シンプル隣接判定テスト
cat("\n=== 隣接判定テスト ===\n")

# ドイツ（最初の10セル）
cat("ドイツ:\n")
de_sample <- head(geo_de, 10)
de_adj <- st_relate(de_sample, de_sample, pattern = "F***T****")
de_neighbors <- sapply(de_adj, length)
cat("隣接数:", paste(de_neighbors, collapse = ", "), "\n")
cat("隣接なし:", sum(de_neighbors == 0), "セル\n")

# 日本（最初の10セル）
cat("\n日本:\n")
jp_sample <- head(geo_jp, 10)
jp_adj <- st_relate(jp_sample, jp_sample, pattern = "F***T****")
jp_neighbors <- sapply(jp_adj, length)
cat("隣接数:", paste(jp_neighbors, collapse = ", "), "\n")
cat("隣接なし:", sum(jp_neighbors == 0), "セル\n")

# 結果
cat("\n判定:", ifelse(sum(de_neighbors == 0) < sum(jp_neighbors == 0), 
                   "ドイツの隣接判定OK", "ドイツに問題あり"), "\n")


