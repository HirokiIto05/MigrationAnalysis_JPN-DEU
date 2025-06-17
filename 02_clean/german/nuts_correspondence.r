# source
source(here("02_clean", "german", "funs_nuts_data.r"))

# Load NUTS correspondence data
df_nuts_raw <- readxl::read_xlsx(
  here(
    "01_data",
    "raw",
    "german",
    "eurostat",
    "NUTS_classification.xlsx"))

df_nuts <- clean_nuts_data(df_nuts_raw) |> 
  dplyr::filter(nuts_level != 2)

df_nuts2 <- clean_nuts_data(df_nuts_raw) |> 
  dplyr::filter(nuts_level == 2)

# Load main data
df_german_foreign <- read.csv(here("01_data", "intermediate", "german", "foreign_master.csv")) |>
  dplyr::filter(!str_detect(county_name, "until")) 

# Identify counties with the same name
list_same_county_name <- df_german_foreign |>
  distinct(county_name) |>
  dplyr::filter(!str_detect(county_name, "until")) |>
  mutate(
    name_letter5 = str_sub(county_name, 1, 7)
  ) |>
  mutate(
    n = n(),
    .by = name_letter5
  ) |>
  dplyr::filter(n > 1) |>  
  distinct(county_name) |>
  pull()  

# Clean county names
df_name_main <- df_german_foreign |>
  mutate(original_name = county_name) |> 
  distinct(county_name, original_name, county_id) |>  
  mutate(
    county_name = if_else(
      county_name %in% list_same_county_name, 
      county_name,
      str_replace_all(county_name, "kreisfreie Stadt", "")),
    county_name = if_else(
      county_name %in% list_same_county_name, 
      county_name,
      str_replace_all(county_name, "Landkreis", "")),
    county_name = if_else(
      county_name %in% list_same_county_name, 
      county_name,
      str_replace_all(county_name, "\\,", "")),
    county_name =
      str_replace_all(county_name, " ", "")
  ) |>
  arrange(county_name) %>%
  mutate(
    num = seq(1:nrow(.)),
    county_name = str_replace_all(county_name, "kreisfreieStadt", "KreisfreieStadt"))

# Create correspondence table
corr_table <- df_name_main |>
  dplyr::mutate(
    county_name = str_replace_all(county_name, "Darmstadt-Dieburg,Landkreis", "Darmstadt-Dieburg"),
    county_name = str_replace_all(county_name, "Heilbronn,KreisfreieStadt", "Heilbronn,Stadtkreis"),
    county_name = str_replace_all(county_name, "Karlsruhe,KreisfreieStadt", "Karlsruhe,Stadtkreis"),
    county_name = str_replace_all(county_name, "Rostock,Landkreis", "LandkreisRostock")
    ) |>
  left_join(df_nuts, by = c("county_name" = "city_name")) |>  
  mutate(
    county_name = if_else(
      is.na(nuts_code),
      paste0(county_name, ",KreisfreieStadt"),
      county_name
    )
  ) |>
  distinct(county_name, original_name, county_id) |>  
  left_join(df_nuts, by = c("county_name" = "city_name")) |>
  mutate(
    county_name = if_else(
      is.na(nuts_code),
      str_replace_all(county_name, ",KreisfreieStadt", ",Landkreis"),
      county_name
    )
  ) |> 
  distinct(county_name, original_name, county_id) |>  
  left_join(df_nuts, by = c("county_name" = "city_name")) |>
  mutate(
    county_name = if_else(
      is.na(nuts_code),
      str_replace_all(county_name, ",Landkreis", ",Stadtkreis"),
      county_name
    )
  ) |>
  distinct(county_name, original_name, county_id) |>  
  left_join(df_nuts, by = c("county_name" = "city_name")) |>  
    mutate(
    county_name = if_else(
      is.na(nuts_code),
      str_replace_all(county_name, ",Stadtkreis", ""),
      county_name
    )
  ) |> 
  distinct(county_name, original_name, county_id) |>  
  left_join(df_nuts, by = c("county_name" = "city_name")) |>
  # Manual modifications to match EUROSTAT
  mutate(
    county_name = case_when(
      county_name == "DillingenanderDonau" ~ "Dillingena.d.Donau",
      county_name == "Friesland" ~ "Friesland(DE)",
      county_name == "Heilbronn" ~ "Heilbronn,Stadtkreis",
      county_name == "MühldorfamInn" ~ "Mühldorfa.Inn",
      county_name == "NeumarktinderOberpfalz" ~ "Neumarkti.d.OPf.",
      county_name == "NeustadtanderAisch-BadWindsheim" ~ "Neustadta.d.Aisch-BadWindsheim",
      county_name == "NeustadtanderWaldnaab" ~ "Neustadta.d.Waldnaab",
      county_name == "PfaffenhofenanderIlm" ~ "Pfaffenhofena.d.Ilm",
      county_name == "Rostock" ~ "Rostock,KreisfreieStadt",
      county_name == "WeideninderOberpfalz" ~ "Weideni.d.Opf,KreisfreieStadt",
      county_name == "WunsiedelimFichtelgebirge" ~ "Wunsiedeli.Fichtelgebirge",
      .default = county_name
    )
  ) |>
  mutate(
    county_name = if_else(original_name == "Hof, kreisfreie Stadt", "Hof,KreisfreieStadt", county_name),
    county_name = if_else(original_name == "Hof, Landkreis", "Hof,Landkreis", county_name)
  ) |>
  distinct(county_name, original_name, county_id) |>  
  left_join(df_nuts, by = c("county_name" = "city_name")) 


table_output <- corr_table |>
  left_join(df_nuts2 |> distinct(nuts_code2, city_name), by = c("nuts_code2" = "nuts_code2")) |>
  select(-county_name) |>
  select(
    county_name = original_name,
    county_id,
    nuts_code2,
    nuts_code3 = nuts_code,
    city_name
  ) |>
  arrange(nuts_code2, nuts_code3)

# Save correspondence table
openxlsx::write.xlsx(
  table_output,
  here("01_data", "intermediate", "german", "nuts_correspondence.xlsx"))
