
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

pdf_file1 <- arquivos[1]
pdf_file2 <- arquivos[2]

output_file <- NULL
tabela_cad_tex(pdf_file1, pdf_file2)
tinytex::latexmk("tabela_cad.tex")

tabela_cad_xlsx(pdf_file1, pdf_file2)

resumo_tex(pdf_file1, pdf_file2)
tinytex::latexmk("resumo.tex")

resumo_xlsx(pdf_file1, pdf_file2)

