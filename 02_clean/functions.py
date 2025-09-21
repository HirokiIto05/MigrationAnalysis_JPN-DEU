import polars as pl

def change_country_names(df):
  mapping = {
    "Egypt": "Egypt, Arab Rep.",
    "Afghanistan, Republic of": "Afghanistan",
    "Bahamas": "Bahamas, The",
    "Bolivia, Plurinational State": "Bolivia",
    "Gambia": "Gambia, The",
    "Hong Kong": "Hong Kong SAR, China",
    "Iran, Islamic Republic of": "Iran, Islamic Rep.",
    "Yemen": "Yemen, Rep.",
    "Kyrgyzstan": "Kyrgyz Republic",
    "Congo, The Democratic Republic of the": "Congo, Dem. Rep.",
    "Congo, Republic of the": "Congo, Rep.",
    "Korea, Democratic People's Republic": "Korea, Dem. Rep.",
    "Korea, Republic of": "Korea, Rep.",
    "Lao People's Democratic Republic": "Lao PDR",
    "Macau": "Macao SAR, China",
    "Micronesia, Federated States of": "Micronesia, Fed. Sts.",
    "Moldova, Republic of": "Moldova",
    "Montenegro (since 2006-06-03)": "Montenegro",
    "Slovakia": "Slovak Republic",
    "Saint Kitts and Nevis": "St. Kitts and Nevis",
    "Saint Lucia": "St. Lucia",
    "Saint Vincent and the Grenadines": "St. Vincent and the Grenadines",
    "Sudan (without South Sudan) (since 2011-07-09)": "Sudan",
    "South Sudan (since 2011-07-09)": "South Sudan",
    "Syrian": "Syrian Arab Republic",
    "Taiwan": "Taiwan, China",
    "Turkey": "Trkiye",
    "Venezuela, Bolivarian Republic of": "Venezuela, RB",
  }
  return df.with_columns(
    pl.col("country_name").replace(mapping).alias("country_name")
  )
  
