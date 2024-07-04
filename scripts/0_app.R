rm(list = ls())

library(pdftools)
library(magrittr)
library(tidyverse)
library(stringr)

source('scripts/funcoes.R')

load('data/cadOrig.RData')
save(cad, file = 'data/cad.RData')

processo <- '23070.022671/2023-56'
save(processo, file = 'data/processo.RData')

notaDirecao <- 9.1
save(notaDirecao, file = 'data/notaDirecao.RData')

# lendo o arquivo (pdf) do Radoc
files <- list.files(path = 'data-raw/',
                    pattern = "pdf$")

arquivo <- paste0('data-raw/',files[6])

# Convertendo em tabela e salvando em arquivo
tabela <- pdf_text(arquivo) %>% readr::read_lines() 
save(tabela, file = 'data/tabela.RData')

# separando os dados do docente
source('scripts/1_docente.R')

source('scripts/2_radoc.R')
source('scripts/3_afastamento.R')

source('scripts/I_ensino.R')
source('scripts/II_producao.R')
source('scripts/III_ativPesqExt.R')
source('scripts/IV_ativAdm.R')
source('scripts/V_outrasAtiv.R')

files <- list.files(path = 'data-raw/',
                    pattern = "pdf$")
arquivo <- paste0('data-raw/',files[7])

# Convertendo em tabela e salvando em arquivo
tabela <- arquivo %>% pdf_text() %>% readr::read_lines() 
save(tabela, file = 'data/tabela.RData')

source('scripts/2_radoc.R')
source('scripts/3_afastamento.R')

source('scripts/I_ensino.R')
source('scripts/II_producao.R')
source('scripts/III_ativPesqExt.R')
source('scripts/IV_ativAdm.R')
source('scripts/V_outrasAtiv.R')

source('scripts/ajusteCad.R')

setwd("texFiles")
tinytex::pdflatex(file = 'tabelaCad.tex')
system2('open', 
        args = 'tabelaCad.pdf', 
        wait = FALSE)
setwd("../")

files <- files[1:4]
alunos <- paste0('data-raw/',files[1])
alunos <- alunos %>% 
  pdftools::pdf_ocr_text(page=1) %>%
  readr::read_lines() 

