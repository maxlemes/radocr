coleta <- function (file){
  #' Extrai os dados do Docente
  #'
  #' @param file arquivo em pdf do RADOC do docente
  #'
  #' @return um dataframe com os dados coletados no arquivo
  #'
  #' @examples
  #'\dontrun{
  #' coleta('data/radoc1.pdf')
  #'}
  #' 
  
  # definindo variáveis locais
  b <- d <- NULL
  listas <- radocr::listas

  # transformando a tabela em texto
  tabela <- readr::read_lines(pdftools::pdf_text(file))
  tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

  # corrigindo erros observados ----------------------------------------------
  tabela <- gsub('IV-2.6.3', 'IV-2-6.3', tabela)

  # extraindo o ano do Radoc
  ano <- tabela[xts::first(which(
    stringr::str_detect(tabela, "^Relat\\u00f3rio do docente")))]
  ano <- as.numeric(stringr::str_sub(
    stringr::str_extract(ano, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),start = 5))

  # coleta a partir do item I - ATIVIDADES DE ENSINO ---------------------------
  ini <- xts::first(which(stringr::str_detect(tabela, '^I ')))

  df <- tabela[ini:length(tabela)]

  # transformando em um data frame
  df <- tabtibble(df)

  # filtrando os itens pontuados
  itens <- which(grepl("^Item", df[[1]]))

  df <- df[itens,1:4]

  df[[2]] <- stringr::str_replace_all(df[[2]], "\\s", "")

  # Arrumando o dataframe
  if (nrow(df) != 0) {
    # converte a ultima coluna em números e eliminas as linhas sem pontos
    df[[ncol(df)]] <- suppressWarnings(as.numeric(df[[ncol(df)]]))

    # descartando itens que não foram pontuados
    df <- df[!is.na(df[,ncol(df)]),]

    # ordena as linhas pelo número do item
    df <- dplyr::arrange(df, stringr::str_rank(b, numeric = TRUE))

    # agrupando e somando os itens
    df <- dplyr::group_by(df, a = b)

    df <- dplyr::summarise(df,
                           b = dplyr::n(),
                           d = sum(d))

    aux <- df
  }

  # coletando o tempo das atividades -------------------------------
  df <- df[(df$a %in% listas[['lista_tempo']]),]

  lista <- df[[1]]

  if (length(lista) > 0) {
    for (i in 1:length(lista)) {
      ini <- which(
        stringr::str_detect(tabela,paste('^Item da Resolu\u00e7\u00e3o:',lista[i])))

      if (length(ini) == 0) {
        ini <- which(
          stringr::str_detect(tabela,paste('^Item da Resolu\u00e7\u00e3o:',
                                           gsub("-", " - ", lista[i]))))
      }

      tempo <- NULL

      for (j in 1:length(ini)) {
        end <- ini[j] + 20
        df <- tabela[ini[j]:end]

        df <- tabtibble(df)

        item <- which(grepl("(^Data)|(^In\u00edcio)", df[[1]]))

        df <- df[item,1:4]

        newyear <- as.Date(paste0('01/01/',ano), format = '%d/%m/%Y')
        inicio <- as.Date(df[[2]][1], format = '%d/%m/%Y')
        termino  <- as.Date(df[[4]][1], format = '%d/%m/%Y')
        reveillon <- as.Date(paste0('31/12/',ano), format = '%d/%m/%Y')

        inicio  <- max(newyear, inicio)

        if (is.na(termino)){
          termino <- reveillon
        } else {
          termino <- min(reveillon, termino)
        }

        tempo[j] <- 1 + as.numeric(format(termino, '%m')) -
          as.numeric(format(inicio, '%m'))

      }

      aux[aux$a == lista[i],'Tempo'] <- sum(tempo)

    }
  } else {
    aux[,'Tempo'] <- as.numeric(NA)
  }

  df <- aux

  colnames(df)[1:3] <- c('Item', 'Qtde', paste0('S.',ano))

  return(df)
}
