load('data/tabela.RData')

# Localizando os dados do docente na tabela
ini <- xts::first(which(stringr::str_detect(tabela, "^Nome")))
end <- xts::first(which(stringr::str_detect(tabela, "^Cargo")))

# convertendo a tabela em um tibble com duas colunas
# primeira coluna
dr1 <- tabela[ini:end] |>
  as_tibble() |>
  extract(value,
          into = "Coluna 1",
          regex = "(^.*[A-Z]?[^\\s](?=\\s\\s))",
          remove = T)

# segunda coluna
dr2 <- tabela[ini[1]:end[1]] %>%
  as_tibble() %>%
  extract(value,
          into = "Coluna 2",
          regex = "((?<=\\s\\s)\\S+(?s).*$)",
          remove = T)
# juntando as duas
dr <-as_tibble(cbind(dr1, dr2))


# as vezes o nome da lotação passa para outra linha
if (is.na(dr[[1]][8])){
  dr[[2]][9] <- paste(dr[[2]][9],dr[[2]][8])
  dr <- dr[-8,]
}


# ajustes finais
dr[[1]][1] <- str_replace(dr[[1]][1], '(^Nome)', 'Docente')
dr[[2]][1] <- dr[[2]][2]
dr[[1]][4] <- str_replace(dr[[1]][4], '(^SIAPE)', 'Matrícula SIAPE')
dr[[2]][8] <- str_replace(dr[[2]][8], '(^Exercício)', 'Unidade')
dr[[2]][5] <- paste(dr[[2]][4],'-', dr[[2]][5])
dr[[2]][4] <- dr[[1]][7]
dr[[1]][6] <- dr[[2]][6]
dr[[2]][6] <- dr[[2]][8]
dr <- dr[-c(2,3,7,8),]

# Salvando em arquivo
docente <- dr
save(docente, file = paste0("data/docente.RData"))


