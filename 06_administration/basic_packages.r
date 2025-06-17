pacman::p_load(
    here,
    dplyr,
    ggplot2,
    tidyr,
    gt,
    gtExtras,
    estimatr,
    modelsummary,
    janitor,
    stringr,
    kableExtra,
    conflicted,
    readr
)

conflicted::conflict_prefer_all("dplyr", quiet = TRUE)
