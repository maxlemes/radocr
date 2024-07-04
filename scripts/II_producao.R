
#-----  II - PRODUÇÃO INTELECTUAL  ---------------------------------------------

load('data/radoc.RData')
load('data/cad.RData')
load('data/anexoII.RData')

source('scripts/funcoes.R')

top <- c("II-1 Produção Científica",
         "II-2 Produção Artística e Cultural",
         "II-3 Produção Técnica ou Tecnológica",
         "II-4 Outro Tipo de Produção",
         "III - ATIVIDADES DE PESQUISA E DE EXTENSÃO")

topCAD <-c('II',   # - PRODUÇÃO INTELECTUAL (Anexo II)',
           'II-1', # - Produção Científica',
           'II-2', # - Produção Artística e Cultural',
           'II-3', # - Produção Técnica ou Tecnológica',
           'II-4' # - Outro Tipo de Produção'
           )

# II-1 Produção Científica -----------------------------------------------------
df <- agrupamento(dt, top[1], top[5])

aux <- df
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[2])

# II-2 Produção Artística e Cultural -------------------------------------------
df <- agrupamento(dt, top[2], top[3])

aux <- rbind(aux,df)
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[3])

# II-3 Produção Técnica ou Tecnológica -----------------------------------------
df <- agrupamento(dt, top[3], top[4])

aux <- rbind(aux,df)
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[4])

# II-4 Outro Tipo de Produção --------------------------------------------------
df <- agrupamento(dt, top[4], top[5])

aux <- rbind(aux,df)

# aux <- left_join(aux, da, by = join_by(description ==  COD))
# aux[,'Pontuação'] <- aux[ ,'amount']*aux[,'Pontos']


# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[5])
cad <- tabelaCad(aux, cad, topCAD[1])
save(cad, file = "data/cad.RData")

# Salvando dados de Produção ---------------------------------------------------

df <- aux[-3]
df <- df %>% bind_rows(summarise_all(., ~if(
  is.numeric(.)){sum(.)} else {"Total"}))


prod <- df
save(prod, file = "data/II_producao.RData")




# rm(list = ls())
