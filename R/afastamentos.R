afastamentos <- function (file){
  #' Afastamentos
  #'
  #' @param file arquivo em pdf do RADOC do docente
  #'
  #' @return um dataframe com os dados do afastamento do docente
  #'
  #' @examples
  #' \dontrun{
  #' afastamentos(tabela)
  #' }

  # transformando o arquivo em tabela e depois em texto
  tabela <- readr::read_lines(pdftools::pdf_text(file))
  tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

  # extraindo o ano do Radoc
  ano <- tabela[xts::first(which(
    stringr::str_detect(tabela, "^Relat\\u00f3rio do docente")))]
  ano <- as.numeric(stringr::str_sub(
    stringr::str_extract(ano, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),start = 5))

  # Localizando os dados do docente na tabela
  ini <- xts::first(which(stringr::str_detect(tabela, "^Afastamentos")))
  end <- xts::first(which(stringr::str_detect(tabela, "^I - ATIVIDADES DE ENSINO")))

  df <- tabela[ini:(end-1)]

  df <- tabtibble(df)

  aux <- df

  # Quando há afastamento
  if (df[[1]][2] != "Nenhum registro."){

    item <- xts::first(which(grepl("(^Data)|(^Início)", df[[1]])))

    df <- df[item:(item+1),1:4]

    newyear <- as.Date(paste0('01/01/',ano), format = '%d/%m/%Y')
    inicio <- as.Date(df[[2]][1], format = '%d/%m/%Y')
    termino  <- as.Date(df[[2]][2], format = '%d/%m/%Y')
    reveillon <- as.Date(paste0('31/12/',ano), format = '%d/%m/%Y')

    inicio  <- max(newyear, inicio)

    if (is.na(termino)){
      termino <- reveillon
    } else {
      termino <- min(reveillon, termino)
    }

    tempo <- as.numeric(1 + termino - inicio)

    df <- aux

    df[1,2] <- df[2,2]
    df[2,1:2] <- df[2,3:4]
    df[3,1] <- 'Per\u00edodo'
    df[3,2] <- paste(df[3,2], 'a', df[4,2])
    df[4,1] <- 'Dias no ano'
    df[4,2] <- as.character(tempo)

    df <- df[,1:2]

  } else {
    # quando não há afastamento
   df[1,2] <- df[2,1]
   df <- df[1,1:2]
  }

  return(df)
}
