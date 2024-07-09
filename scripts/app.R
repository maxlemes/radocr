
rm(list = ls())
# lendo os arquivos (pdf) da pasta data-raw
files <- list.files(path = 'data-raw/',
                    pattern = "pdf$")

arquivos <- paste0('data-raw/',files[-1])

# Testando os arquivos
filesCheck(arquivos)

file <- arquivos[2]

#----- Afastamentos ------------------------------------------------------------
afastamentos(file)

docente(file)

df <- coleta(file)

print(coleta(file), n=30)

df <- coleta(file)

pontua(df)
