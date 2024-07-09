pontua <- function(df) {

  aux <- df
  colnames(df)[1] <- 'COD'

  df <- dplyr::left_join(df, anexoII)

  # ajustando os itens limitados a 1 por ano
  df[(df$COD %in% lista1), 'b'] <- 1

  # pontuando os itens fora da lista0 sem limitação
  df[!(df$COD %in% lista0),gsub('c.', '', colnames(df)[4])] <-
    df[!(df$COD %in% lista0),2] * df[!(df$COD %in% lista0),6]

  # Aproveitando a pontuação de Ensino
  df[df$COD %in% c('I-1-1', 'I-1-2', 'I-2-1', 'I-2-2'),7] <-
    df[df$COD %in% c('I-1-1', 'I-1-2', 'I-2-1', 'I-2-2'),3]

  # pontuando os itens da lista0 sem limitação
  df[(df$COD %in% lista0),7] <-
    df[(df$COD %in% lista0),4] * df[(df$COD %in% lista0),6]

  # pontuando os itens com pontuação atribuida a cada ano de atividade
  df[(df$COD %in% lista_anual),7] <- df[(df$COD %in% lista_anual),7]/12

  df[,7] <- round(df[,7],2)

  # ajustando as limitacoes
  df[(df$COD %in% lista),7] <- df[(df$COD %in% lista_anual),7]/12




  df[,'TESTE'] <- df[,3] == round(df[,7],2)



  # Localizando os dados do docente no dataframe
  ini <- xts::first(which(stringr::str_detect(tabela, "^Afastamentos")))
  end <- xts::first(which(stringr::str_detect(tabela, "^I - ATIVIDADES DE ENSINO")))

  df <- tabela[ini:(end-1)]

  df <- tabtibble(df)

}
