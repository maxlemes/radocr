docente <- function (file){
  #' Extrai os dados do Docente
  #'
  #' @param file arquivo em pdf do RADOC do docente
  #'
  #' @return um dataframe com os dados do docente
  #'
  #' @examples
  #' \dontrun{
  #' docente(tabela)
  #' }

  # transformando o arquivo em tabela e depois em texto
  tabela <- readr::read_lines(pdftools::pdf_text(file))
  tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

  # Localizando os dados do docente na tabela
  ini <- xts::first(which(stringr::str_detect(tabela, "^Nome")))
  end <- xts::first(which(stringr::str_detect(tabela, "^Cargo")))

  df <- tabela[ini:end]

  df <- tabtibble(df)

  # as vezes o nome da lotação passa para outra linha
  if (is.na(df[[2]][8])){
    df[[4]][7] <- paste(df[[4]][7],df[[1]][8])
    df[[4]][9] <-  df[[4]][7]
    df <- df[-8,]
  }

  df <- df[-3,]

  # ajustes finais
  df[[1]][1] <- 'Docente'
  df[[1]][3] <- 'Matr\u00edcula SIAPE'
  df[[3]][6] <- 'Unidade'
  df[[4]][3] <- paste(df[[4]][3], '- N\u00edvel', df[[4]][4])
  df[[3]][4] <- df[[1]][7]
  df[[4]][4] <- df[[2]][7]
  df <- df[-7,]

  return(df)
}
