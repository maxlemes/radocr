
load('data/radoc.RData')
load('data/cad.RData')


# Afastamentos -----------------------------------------------------------------
ini <- which(grepl("Afastamentos", dt[[1]]))
end <- which(grepl("I - ATIVIDADES DE ENSINO", dt[[1]])) -1

df <- dt[ini:end,1]
afast <- df
save(afast, file = paste0("data/afastamentos.Rdata"))
