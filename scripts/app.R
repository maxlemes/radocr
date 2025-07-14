
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

arquivos <- paste0('data-raw/',files[c(9,10)])

# Testando os arquivos
# filesCheck(arquivos)

pdf_file1 <- arquivos[1]
pdf_file2 <- arquivos[2]
pdf_file3 <- arquivos[8]

pdf_files <- c(pdf_file1, pdf_file2)
pdf_file <- pdf_files[1]

output_file <- NULL
tabela_cad_tex(pdf_file1, pdf_file2)
tinytex::latexmk("tabela_cad.tex")

tabela_cad(pdf_file1, pdf_file2)

resumo_tex(pdf_file1, pdf_file2)
tinytex::latexmk("resumo.tex")

resumo_xlsx(pdf_file1, pdf_file2)

# pdf_files <- c(pdf_file1, pdf_file2, pdf_file1)
docente(pdf_files)
afastamentos(pdf_file2)
afastamentos(arquivos[7])

tab_cad(pdf_file1, pdf_file2, pdf_file3)

my_func <- function(...) {   
  dots <- c(...)   
  dots_chr <- unlist(dots)
  pdf_file1 <- dots[[1]]
  pdf_file2 <- dots[[2]]
  filesCheck(pdf_file1, pdf_file2, pdf_file2)
  print(dots)
  # print(dots_chr)
}  

my_func(pdf_file1, pdf_file2)

