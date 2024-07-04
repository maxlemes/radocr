
#-----  V - OUTRAS ATIVIDADES  -------------------------------------------------

load('data/radoc.RData')
load('data/cad.RData')

source('scripts/funcoes.R')

top <- c("V-1 Atividades Acadêmicas - Orientação",
         "V-2 Atividades Acadêmicas - Bancas e Cursos",
         "V-3 Atividades de Aprendizado e Aperfeiçoamento")

topCAD <-c('V',   # - OUTRAS ATIVIDADES (Anexo II)',
           'V-1', # - Atividades Acadêmicas ± Orientação',
           'V-2', # - Atividades Acadêmicas ± Bancas e Cursos',
           'V-3' # - Atividades de Aprendizado e Aperfeiçoamento'
           )

# V-1 Atividades Acadêmicas - Orientação ---------------------------------------
df <- atividades(dt, top[1], top[2])

aux <- df
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[2])

# V-2 Atividades Acadêmicas - Bancas e Cursos ----------------------------------
df <- atividades(dt, top[2], top[3])

aux <- rbind(aux,df)
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[3])


# V-3 Atividades de Aprendizado e Aperfeiçoamento ------------------------------
df <- atividades(dt, top[3], top[4])

aux <- rbind(aux,df)
# Atualizando a tabela da CAD ------
cad <- tabelaCad(df, cad, topCAD[4])
cad <- tabelaCad(aux, cad, topCAD[1])
save(cad, file = "data/cad.RData")

# Salvando em arquivo ----------------------------------------------------------
df <- aux[-3]
df <- df %>% bind_rows(summarise_all(., ~if(
  is.numeric(.)){sum(.)} else {"Total"}))

outras <- df

save(outras, file = "data/V_outrasAtiv.RData")


# Ajustes na tabela da CAD -----------------------------------------------------
df <- cad
I <- df[[1]]%>% str_detect("^I") %>% which %>% first
II <- df[[1]]%>% str_detect("^II") %>% which %>% first
III <- df[[1]]%>% str_detect("^III") %>% which %>% first
IV <- df[[1]]%>% str_detect("^IV") %>% which %>% first
V <- df[[1]]%>% str_detect("^V") %>% which %>% first
P <- df[[1]]%>% str_detect("^P") %>% which
NF <- df[[1]]%>% str_detect("^NF") %>% which
S <- df[[1]]%>% str_detect("^S") %>% which

df[[ncol(df)]][P] <- sum(df[c(I,II,III,IV,V),ncol(df)])
df[[ncol(df)]][NF] <- round(min(10,df[[ncol(df)]][P]/32),2)
df[[ncol(df)]][S] <- sum(df[c(I,III,IV,V),ncol(df)])

cad <- df
save(cad, file = "data/cad.RData")

