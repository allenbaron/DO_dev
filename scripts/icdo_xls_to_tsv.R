# convert ICD-O xls file to tsv for version control

library(here)
library(readxl)
library(readr)

icdo <- readxl::read_excel(
    here::here("data/ICD-O-3.2_final_15112019.xls"),
    skip = 1,
    col_types = "text"
)

readr::write_tsv(
    icdo,
    here::here("data/ICD-O-3.2_final_15112019.tsv")
)
