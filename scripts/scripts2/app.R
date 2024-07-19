
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

file1 <- arquivos[7]
file2 <- arquivos[8]

output_file <- "tabela_cad.tex"
tabela_cad_tex(file1, file2)
tinytex::latexmk(output_file)

output_file <- "tabela_cad.xlsx"
tabela_cad_xlsx(file1, file2)


# Caminho para o arquivo que você deseja verificar
caminho_arquivo <- "R/tabela_cad.R"

# Ler o conteúdo do arquivo como uma única string
conteudo <- readLines(caminho_arquivo, warn = FALSE)

# Função para verificar se uma string contém caracteres não-ASCII
contem_nao_ascii <- function(string) {
  any(grepl("[^[:ascii:]]", string, perl = TRUE))
}

# Verificar se o conteúdo do arquivo contém caracteres não-ASCII
if (contem_nao_ascii(conteudo)) {
  cat("O arquivo contém caracteres não-ASCII.\n")
} else {
  cat("O arquivo NÃO contém caracteres não-ASCII.\n")
}
