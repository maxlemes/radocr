#
# lendo o arquivo (pdf) do Radoc
files <- list.files(path = 'data-raw/',
                    pattern = "pdf$")

arquivos <- paste0('data-raw/',files[c(2,3)])

# Testando os arquivos
filesCheck(file1 = arquivos[1], file2 = arquivos[2])

file <- arquivos[2]

# transformando a tabela em texto
tabela <- readr::read_lines(pdftools::pdf_text(file))
tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

# extraindo o ano do Radoc
ano <- tabela[xts::first(which(
  stringr::str_detect(tabela, "^Relatório do docente")))]
ano <- as.numeric(stringr::str_sub(
  stringr::str_extract(ano, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),start = 5))

#----- Afastamentos ------------------------------------------------------------
df <- tabela
ini <- xts::first(which(stringr::str_detect(df, "^Afastamentos")))

df <- df[(ini + 1):(ini + 3)]

df <- stringr::str_replace_all(df, ":", "|")
df <- stringr::str_replace_all(df,  "\\s{2,}", "|")
df <- tibble::as_tibble(df)

df <- tidyr::separate_wider_delim(df,
                                  value,
                                  delim = '|',
                                  names = letters[1:4],
                                  cols_remove = TRUE,
                                  too_few = "align_start",
                                  too_many = "merge")

df[[2]] <- stringr::str_replace_all(df[[2]], "^\\s+|\\s+$", "")
df[[4]] <- stringr::str_replace_all(df[[4]], "^\\s+|\\s+$", "")

inicio <- as.Date(df[[2]][2], format = '%d/%m/%Y')
termino  <- as.Date(df[[2]][3], format = '%d/%m/%Y')
reveillon <- as.Date(paste0('31/12/',ano), format = '%d/%m/%Y')


df[2,1] <- df[1,3]
df[2,2] <- df[1,4]
df[3,1] <- 'Dias no ano'
df[3,2] <- as.character(min(as.numeric(termino + 1 - inicio),
                            as.numeric(reveillon - inicio)))
df <- df[,1:2]

# salvando o RADOC------------------------------------------
dt <- dt[,1:4]
colnames(dt) <- c('description', 'amount', 'time', 'points')
save(dt, file = 'data/radoc.RData')


# Atualizando a tabela da CAD --------------------------------------------------
cad[, ano] <- as.numeric('')

# salvando a tabela da CAD -----------------------------------------------------
save(cad, file = 'data/cad.RData')

