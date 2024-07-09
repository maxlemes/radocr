
rm(list = ls())
# lendo os arquivos (pdf) da pasta data-raw
files <- list.files(path = 'data-raw/',
                    pattern = "pdf$")

arquivos <- paste0('data-raw/',files[-1])

# Testando os arquivos
filesCheck(arquivos)

file <- arquivos[1]
prod(file)

#----- RADOC ------------------------------------------------------------

# transformando a tabela em texto
tabela <- readr::read_lines(pdftools::pdf_text(file))
tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

# stringi::stri_escape_unicode('ó')
# extraindo o ano do Radoc
ano <- tabela[xts::first(which(
  stringr::str_detect(tabela, "^Relat\\u00f3rio do docente")))]
ano <- as.numeric(stringr::str_sub(
  stringr::str_extract(ano, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),start = 5))

#----- Afastamentos ------------------------------------------------------------
afastamentos(tabela)

docente(tabela)

