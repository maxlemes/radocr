
rm(list = ls())
library(devtools)

#
#   encontra o unicode do caracter
#   stringi::stri_escape_unicode('ção')
#   saida: "rea\\u00e7\\u00e3o"
#
#   Para usar lembrar de tirar uma /


# lendo os arquivos (pdf) da pasta data-raw
files <- list.files(path = 'data-raw/',
                    pattern = "pdf$")

arquivos <- paste0('data-raw/',files[-c(1,10)])

# Testando os arquivos
# filesCheck(arquivos)

file <- arquivos[1]

#----- Afastamentos ------------------------------------------------------------
# afastamentos(file)
#
# docente(file)

df <- coleta(file)
coleta(file)
pontua(df)


# print(coleta(file), n = 30)

print(pontua(df), n = 30)

df <- coleta(file)

pontua(df)
