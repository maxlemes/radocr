pontua <- function(file) {
  #' Pontua os dados do RADOC seguindo o Anexo II da Resolução Consuni 18/2017
  #'
  #' @param file arquivo em pdf RADOC gerado pelo SICAD+
  #'
  #' @return um dataframe com os dados e suas respectivas pontuações
  #'
  #' @examples
  #' \dontrun{
  #' pontua(df)
  #' }

  # aux <- df

  df <- coleta(file)

  df <- suppressMessages(
    dplyr::left_join(df, anexoII, by = "Item")
    )
  # ajustes nos dados da descrição
  df[[5]] <- stringr::str_replace_all(df[[5]], "^\\s+|\\s+$", "")

  ano <- gsub('S.', '', colnames(df)[3])
  colnames(df)[3] <- 'SICAD'

  # ajustando os itens limitados a 1 por ano
  df[(df[['Item']] %in% listas[['lista1']]), 'Qtde'] <- 1

  # pontuando os itens (fora da lista_tempo) por quantidade
  df[!(df[['Item']] %in% listas[['lista_tempo']]), ano] <-
    df[!(df[['Item']] %in% listas[['lista_tempo']]), 'Qtde'] *
    df[!(df[['Item']] %in% listas[['lista_tempo']]), 'Pontos']

  # Aproveitando a pontuação de Ensino do SICAD+
  df[df[['Item']] %in% c('I-1-1', 'I-1-2', 'I-2-1', 'I-2-2'),  ano] <-
    df[df[['Item']] %in% c('I-1-1', 'I-1-2', 'I-2-1', 'I-2-2'), 'SICAD']

  # pontuando os itens (da lista_tempo) por tempo
  df[(df[['Item']] %in% listas[['lista_tempo']]), ano] <-
    df[(df[['Item']] %in% listas[['lista_tempo']]), 'Tempo'] *
    df[(df[['Item']] %in% listas[['lista_tempo']]), 'Pontos']

  # pontuando os itens com pontuação atribuida a cada ano de atividade
  df[(df[['Item']] %in% listas[['lista_anual']]), ano] <-
    df[(df[['Item']] %in% listas[['lista_anual']]), ano]/12

  # arrendondano para 2 casas decimais
  df[, ano] <- round(df[, ano], 2)

  # ajustando as limitacoes
  for (i in 3:12){
    if (nrow(df[(df[['Item']] %in% listas[[i]]), ]) != 0) {

      limite <- as.numeric(gsub('lista', '', names(listas[i])))

      df[[ano]][(df[['Item']] %in% listas[[i]])] <-
        ifelse(df[[ano]][(df[['Item']] %in% listas[[i]])] < limite,
               df[[ano]][(df[['Item']] %in% listas[[i]])],
               limite)
    }
  }

  df[,ano] <- round(df[,ano],1)
  df <- df[,c(1,5,2,7)]

  return(df)
}
