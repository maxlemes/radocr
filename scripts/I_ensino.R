
#-----  I - ATIVIDADES DE ENSINO  ----------------------------------------------

load('data/radoc.RData')
load('data/cad.RData')

source('scripts/funcoes.R')

top <- c("I-1 Ensino Básico",
         "I-1 Graduação",
         "I-2 Pós-Graduação Stricto Sensu",
         "I-2 Pós-Graduação Latu Sensu",
         "I-3 Projetos de Ensino",
         "II - PRODUÇÃO INTELECTUAL")

topCAD <-c('I - ATIVIDADES DE ENSINO (Anexo II)',
           'I-1 - Ensino Básico ou de Graduação',
           'I-2 - Ensino de Pós-Graduação',
           'I-3 - Projetos de Ensino')

topCAD <-c('I',   # - ATIVIDADES DE ENSINO (Anexo II)',
           'I-1', # - Ensino Básico ou de Graduação',
           'I-2', # - Ensino de Pós-Graduação',
           'I-3'  # - Projetos de Ensino'
           )

# I-1 Ensino Básico ------------------------------------------------------------
df1 <- agrupamento(dt, top[1], top[2])

# I-1 Graduação ---------------------
df2 <- agrupamento(dt, top[2], top[3])

df <- rbind(df1,df2)
aux <- df

# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[2])

# I-2 Pós-Graduação Stricto Sensu ----------------------------------------------
df1 <- agrupamento(dt, top[3], top[4])

# I-2 Pós-Graduação Latu Sensu -------˙˙˙
df2 <- agrupamento(dt, top[4], top[5])

df <- rbind(df1,df2)
aux <- rbind(aux,df)

# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[3])

# I-3 Projetos de Ensino -------------------------------------------------------
df <- agrupamento(dt, top[5], top[6])
aux <- rbind(aux,df)

# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[4])
cad <- tabelaCad(aux, cad, topCAD[1])
save(cad, file = "data/cad.RData")

# Salvando dados de Ensino -----------------------------------------------------
df <- aux
df <- df %>% bind_rows(summarise_all(., ~if(
  is.numeric(.)){sum(.)} else {"Total"}))

ensino <- df
save(ensino, file = "data/I_ensino.RData")

# rm(list = ls())
