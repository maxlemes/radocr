
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

file1 <- arquivos[5]
file2 <- arquivos[6]

tabela_cad(file1,file2)


file3 <- NA
#----- Afastamentos ------------------------------------------------------------
afastamentos(file2)
#
# docente(file)


pontua(file1)

tabela_cad(file2,file1)

setwd("tabelas")
system2('pdflatex', 
        args = 'tabelaCad.tex', 
        wait = FALSE)
system2('open', 
        args = 'tabelaCad.pdf', 
        wait = FALSE)
setwd("../")


# abrindo a planilha


o# print(coleta(file), n = 30)

print(pontua(df), n = 30)

df <- coleta(file)

pontua(df)

https://guslipkin.medium.com/making-pretty-excel-files-in-r-46a15c7a2ee8
