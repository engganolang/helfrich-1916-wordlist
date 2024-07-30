# This is the code to retrieve the original data in Google Spreadsheet and save it into .rds file
library(googlesheets4)
library(tidyverse)

# load Helfrich's (1916) wordlist ======
sheet_url = "https://docs.google.com/spreadsheets/d/1TfCQ0ctLtqLxnmOZ8MJAcKcoiG9GdbCLidJPqAA4N4g/edit?usp=sharing"
helfrich_wl <- googlesheets4::read_sheet(sheet_url, sheet = 1, col_types = "iiccccccccccc")
glimpse(helfrich_wl)
helfrich_wl %>% write_rds("helfrich_wl.rds")
