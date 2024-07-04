
#----- "IV - ATIVIDADES ADMINISTRATIVAS E DE REPRESENTAÇÃO" --------------------

load('data/radoc.RData')
load('data/cad.RData')

source('scripts/funcoes.R')

top <- c("IV-1 Direção e Função Gratificada",
         "IV–2 Atividades Administrativas",
         "IV-3 Outras Atividades Administrativas",
         'IV-4 Atividades de Representação Fora da UFG',
         'V - OUTRAS ATIVIDADES')

topCAD <-c('IV', # - ATIVIDADES ADMINISTRATIVAS E DE REPRESENTAÇÃO (Anexo II)',
           'IV-1', # - Direção e Função Gratificada',
           'IV-2', # - Atividades Administrativas',
           'IV-3', # - Outras Atividades Administrativas',
           'IV-4' # - Atividades de Representação Fora da UFG'
           )

# IV-1 Direção e Função Gratificada --------------------------------------------
df <- atividades(dt, top[1], top[2])

aux <- df
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[2])

# IV–2 Atividades Administrativas ----------------------------------------------
df <- atividades(dt, top[2], top[3])

aux <- rbind(aux,df)
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[3])

# IV-3 Outras Atividades Administrativas ---------------------------------------
df <- atividades(dt, top[3], top[4])

aux <- rbind(aux,df)
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[4])

# IV-4 Atividades de Representação Fora da UFG ---------------------------------
df <- atividades(dt, top[4], top[5])

aux <- rbind(aux,df)
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[5])
cad <- tabelaCad(aux, cad, topCAD[1])
save(cad, file = "data/cad.RData")

# Salvando em arquivo ----------------------------------------------------------
df <- aux
df <- df %>% bind_rows(summarise_all(., ~if(
  is.numeric(.)){sum(.)} else {"Total"}))

adm <- df
save(adm, file = paste0("data/IV_ativAdm.RData"))

# rm(list = ls())
