
rm(list = ls())
# lendo os arquivos (pdf) da pasta data-raw
files <- list.files(path = 'data-raw/',
                    pattern = "pdf$")

arquivos <- paste0('data-raw/',files[c(2,3)])

# Testando os arquivos
filesCheck(file1 = arquivos[1], file2 = arquivos[2])

file <- arquivos[1]

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

df <- tabela

afastamentos(tabela)

ini <- xts::first(which(stringr::str_detect(df, "^Afastamentos")))

df <- df[(ini):(ini + 3)]

df <- stringr::str_replace_all(df, ":", "|")
df <- stringr::str_replace_all(df,  "\\s{2,}", "|")
df <- tibble::as_tibble(df)

df <- tidyr::separate_wider_delim(data = df,
                                  cols = .data$value,
                                  delim = '|',
                                  names_sep = '|',
                                  cols_remove = TRUE,
                                  too_few = "align_start",
                                  # too_many = "merge",
)

for(i in 1:4){
  df[[i]] <- stringr::str_replace_all(df[[i]], "^\\s+|\\s+$", "")
}

