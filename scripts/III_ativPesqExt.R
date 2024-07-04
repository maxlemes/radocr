
#-----  "III - ATIVIDADES DE PESQUISA E DE EXTENSÃO"  --------------------------

load('data/radoc.RData')
load('data/cad.RData')

source('scripts/funcoes.R')

top <- c("III-1 Atividades de Pesquisa",
         "III-2 Atividades de Extensão",
         "IV - ATIVIDADES ADMINISTRATIVAS E DE REPRESENTAÇÃO")

topCAD <-c('III',   # - ATIVIDADES DE PESQUISA E DE EXTENSÃO (Anexo II)',
           'III-1', # - Atividades de Pesquisa',
           'III-2' # - Atividades de Extensão'
           )

# III-1 Atividades de Pesquisa -------------------------------------------------
df <- atividades(dt, top[1], top[2])

aux <- df
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[2])

# III-2 Atividades de Extensão -------------------------------------------------
df <- atividades(dt, top[2], top[3])

aux <- rbind(aux,df)
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[3])
cad <- tabelaCad(aux, cad, topCAD[1])
save(cad, file = "data/cad.RData")

# Salvando em arquivo ----------------------------------------------------------
df <- aux
df <- df %>% bind_rows(summarise_all(., ~if(
  is.numeric(.)){sum(.)} else {"Total"}))

ext <- df
save(ext, file = paste0("data/III_ativPesqExt.RData"))

# rm(list = ls())
