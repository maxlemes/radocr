## code to prepare `DATASET` dataset goes here

source('data-raw/orig_cad.R')
source('data-raw/orig_anexoII.R')
source('data-raw/orig_listas.R')
source('data-raw/orig_topicos.R')

usethis::use_data(orig_cad, anexoII, listas, topicos, internal = TRUE, overwrite = TRUE)
