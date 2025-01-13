#
# ------------------- Função INTERNA - tab_cad  -------------------------------
#
#' Extrai os dados e retorna um dataframe com a Tabela da CAD
#' 
#' @description
#' A função `tab_cad()` extrai os dados dos RADOC, utiliza o Anexo II para 
#' pontuar
#' as informações e retorna um dataframe com a Tabela CAD preenchida.
#' 
#' @details
#' Esta função utiliza duas funções internas: uma para coletar os dados e outra 
#' para pontuar as informações.
#' 
#' @param pdf_files Um ou mais arquivos em PDF contendo RADOCs gerados pelo 
#' SICAD+.
#'
#' @return Um dataframe contendo a Tabela CAD preenchida.
#'
#' @examples
#' \dontrun{
#' # Exemplo de uso da função tab_cad
#' tab_cad(pdf_file1, pdf_file2)
#' }
#' @keywords internal
tab_cad <- function(...) {

  pdf_files <- c(...)

  # analisando a carga horária do docente
  doc <- dados_docente(pdf_files[1])
  n <- ifelse(doc[[2]][6] == "20 Horas", 20, 40)


  # capturando a tabela vazia
  cad <- orig_cad

  # montando o dataframe com os dados
  for (file in pdf_files) {
    df <- pontua(file)

    cad[, colnames(df)[4]] <- as.numeric(NA) # Se 2 arquivos são do mesmo ano apenas o último será considerado

    lista_itens <- cad[[1]][1:21]

    for (item in lista_itens) {
      itens <- which(stringr::str_detect(df[[1]], paste0("^", item, "-")))
      dc <- df[itens, ]
      cad[cad[[1]] == item, ncol(cad)] <- sum(dc[[4]])
    }
  }

  # ordenando os anos (caso os arquivos sejam inserido em ordem errada)
  anos <- as.numeric(colnames(cad)[3:ncol(cad)])
  ordem <- order(anos)
  cad <-cad[,c(1:2, ordem + 2)]

  # somando as pontuacoes dos anos analisados
  if (ncol(cad) > 3){
    cad[, "Total"] <- rowSums(cad[, 3:ncol(cad)])
  } 
  else {
    cad[, "Total"] <- cad[, ncol(cad)]
  }
  

  # calculando os valores de P e S
  for (j in 3:ncol(cad)){
    cad[cad[[1]] == "P", j] <- sum(cad[cad[[1]] %in% c("I", "II", "III", "IV","V"), j])
  cad[cad[[1]] == "S", j] <- sum(cad[cad[[1]] %in% c("I", "III", "IV", "V"), j])

  }

  # calculando a nota da CAD
  if (n == 40) {
    cad[cad[[1]] == "NF", ncol(cad)] <- min(cad[cad[[1]] == "P", ncol(cad)] / 32, 10)
  } else {
    cad[cad[[1]] == "NF", ncol(cad)] <- min(cad[cad[[1]] == "P", ncol(cad)] / 20, 10)
  }

  # ajustes finais
  cad[,ncol(cad)] <- round(cad[,ncol(cad)],1)
  for (i in 3:ncol(cad)) {
    cad[[i]] <- as.character(format(cad[[i]], nsmall = 1))
    cad[[i]] <- stringr::str_replace_all(cad[[i]], "^\\s+|\\s+$", "")
    cad[[i]] <- sub(".", ",", cad[[i]], fixed = TRUE)
  }

  cad[cad[[1]] %in% c("NF"), 3:(ncol(cad)-1)] <- "-"

  return(cad)
}


#
# ------------------- Função INTERNA - coleta  -------------------------------
#
#' Extrai os dados a serem pontuados no RADOC
#'
#' @param pdf_file arquivo em pdf do RADOC do docente
#'
#' @return um dataframe com os dados coletados no arquivo
#'
#' @examples
#' \dontrun{
#' coleta('data-raw/radoc1.pdf')
#' }
#' @keywords internal
coleta <- function(pdf_file) {

  # definindo variáveis locais
  b <- d <- NULL
  listas <- listas

  # transformando a tabela em texto
  tabela <- readr::read_lines(pdftools::pdf_text(pdf_file))
  tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

  # corrigindo erros observados ----------------------------------------------

  # Corrigir pontos entre números romanos e o primeiro número
  tabela <- gsub("([IV]+)\\.(\\d+)-(\\d+)", "\\1-\\2-\\3", tabela)

  # Corrigir pontos entre números subsequentes
  tabela <- gsub("([IV]+)-(\\d+)\\.?(\\d+)", "\\1-\\2-\\3", tabela )
   
  # Corrigir pontos entre os dois
  tabela <- gsub("([IV]+)\\.(\\d+)\\.(\\d+)", "\\1-\\2-\\3", tabela )


  # extraindo o ano do Radoc ----------------------------------------------
  ano <- tabela[xts::first(which(
    stringr::str_detect(tabela, "^Relat\u00f3rio do docente")
  ))]
  ano <- as.numeric(stringr::str_sub(
    stringr::str_extract(ano, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
    start = 5
  ))

  # coleta a partir do item I - ATIVIDADES DE ENSINO ---------------------------
  ini <- xts::first(which(stringr::str_detect(tabela, "^I ")))

  df <- tabela[ini:length(tabela)]

  # transformando em um data frame
  df <- tabtibble(df)

  # filtrando os itens pontuados
  itens <- which(grepl("^Item", df[[1]]))

  # selecionando apenas os itens a serem pontuados
  df <- df[itens, 1:4]

  # tirando os espaços em branco da coluna 2
  df[[2]] <- stringr::str_replace_all(df[[2]], "\\s", "")


  # Arrumando o dataframe
  if (nrow(df) != 0) {
    # converte a ultima coluna em números e eliminas as linhas sem pontos
    df[[ncol(df)]] <- suppressWarnings(as.numeric(df[[ncol(df)]]))

    # descartando itens que não foram pontuados
    df <- df[!is.na(df[, ncol(df)]), ]

    # ordena as linhas pelo número do item
    df <- dplyr::arrange(df, stringr::str_rank(b, numeric = TRUE))

    # agrupando e somando os itens
    df <- dplyr::group_by(df, a = b)

    df <- dplyr::summarise(df,
      b = dplyr::n(),
      d = sum(d)
    )

    aux <- df
  }

  # coletando o tempo das atividades -------------------------------
  df <- df[(df[['a']] %in% listas[["lista_tempo"]]), ]

  lista <- df[[1]]

  if (length(lista) > 0) {
    for (i in 1:length(lista)) {
      ini <- which(
        stringr::str_detect(tabela, paste("^Item da Resolu\u00e7\u00e3o:", lista[i]))
      )

      if (length(ini) == 0) {
        ini <- which(
          stringr::str_detect(tabela, paste(
            "^Item da Resolu\u00e7\u00e3o:",
            gsub("-", " - ", lista[i])
          ))
        )
      }

      tempo <- NULL

      for (j in 1:length(ini)) {
        end <- ini[j] + 50
        df <- tabela[ini[j]:end]

        df <- tabtibble(df)

        item <- which(grepl("(^Data)|(^In\u00edcio)", df[[1]]))

        df <- df[item, 1:4]

        newyear <- as.Date(paste0("01/01/", ano), format = "%d/%m/%Y")
        inicio <- as.Date(df[[2]][1], format = "%d/%m/%Y")
        termino <- as.Date(df[[4]][1], format = "%d/%m/%Y")
        reveillon <- as.Date(paste0("31/12/", ano), format = "%d/%m/%Y")

        inicio <- max(newyear, inicio)

        if (is.na(termino)) {
          termino <- reveillon
        } else {
          termino <- min(reveillon, termino)
        }

        tempo[j] <- 1 + as.numeric(format(termino, "%m")) -
          as.numeric(format(inicio, "%m"))
      }
      aux[aux$a == lista[i], "Tempo"] <- sum(tempo)
    }
  } else {
    aux[, "Tempo"] <- as.numeric(NA)
  }

  df <- aux

  colnames(df)[1:3] <- c("Item", "Qtde", paste0("S.", ano))

  return(df)
}


  #
  # -------------------  Função INTERNA - pontua  ------------------------------
  #
  #' Pontua os dados do RADOC seguindo o Anexo II da Resolução Consuni 18/2017
  #'
  #' @param pdf_file arquivo em pdf RADOC gerado pelo SICAD+
  #'
  #' @return um dataframe com os dados e suas respectivas pontuações
  #'
  #' @examples
  #' \dontrun{
  #' pontua(pdf_file)
  #' }
  pontua <- function(pdf_file) {

  
    # definindo variáveis locais
    listas  <- listas
    anexoII <- anexoII
  
    df <- coleta(pdf_file)
  
    df <- suppressMessages(
      dplyr::left_join(df, anexoII, by = "Item")
    )
    # ajustes nos dados da descrição
    df[[5]] <- stringr::str_replace_all(df[[5]], "^\\s+|\\s+$", "")
  
    ano <- gsub("S.", "", colnames(df)[3])
    colnames(df)[3] <- "SICAD"
  
    # ajustando os itens limitados a 1 por ano
    df[(df[["Item"]] %in% listas[["lista1"]]), "Qtde"] <- 1
  
    # pontuando os itens (fora da lista_tempo) por quantidade
    df[!(df[["Item"]] %in% listas[["lista_tempo"]]), ano] <-
      df[!(df[["Item"]] %in% listas[["lista_tempo"]]), "Qtde"] *
        df[!(df[["Item"]] %in% listas[["lista_tempo"]]), "Pontos"]
  
    # Aproveitando a pontuação de Ensino do SICAD+
    df[df[["Item"]] %in% c("I-1-1", "I-1-2", "I-2-1", "I-2-2"), ano] <-
      df[df[["Item"]] %in% c("I-1-1", "I-1-2", "I-2-1", "I-2-2"), "SICAD"]
  
    # pontuando os itens (da lista_tempo) por tempo
    df[(df[["Item"]] %in% listas[["lista_tempo"]]), ano] <-
      df[(df[["Item"]] %in% listas[["lista_tempo"]]), "Tempo"] *
        df[(df[["Item"]] %in% listas[["lista_tempo"]]), "Pontos"]
    
    # Aproveitando a PONTUAÇÃO Dos itens sem data (erro no SICAD+)
    df[df[["Item"]] %in% c("V-2-11"), ano] <-
      df[df[["Item"]] %in% c("V-2-11"), "SICAD"]    

    # Aproveitando a PONTUAÇÃO do SICAD+
    df[df[["Item"]] %in% c("V-3-1",  "V-3-2",  "V-3-3"), ano] <-
      df[df[["Item"]] %in% c("V-3-1",  "V-3-2",  "V-3-3"), "SICAD"]  
  
    # pontuando os itens com pontuação atribuida a cada ano de atividade
    df[(df[["Item"]] %in% listas[["lista_anual"]]), ano] <-
      df[(df[["Item"]] %in% listas[["lista_anual"]]), ano] / 12
  
    # arrendondano para 2 casas decimais
    df[, ano] <- round(df[, ano], 2)
  
    # ajustando as limitacoes
    for (i in 3:12) {
      if (nrow(df[(df[["Item"]] %in% listas[[i]]), ]) != 0) {
        limite <- as.numeric(gsub("lista", "", names(listas[i])))
  
        df[[ano]][(df[["Item"]] %in% listas[[i]])] <-
          ifelse(df[[ano]][(df[["Item"]] %in% listas[[i]])] < limite,
            df[[ano]][(df[["Item"]] %in% listas[[i]])],
            limite
          )
      }
    }
  
    df[, ano] <- round(df[, ano], 1)
    df <- df[, c(1, 5, 2, 7)]
  
    return(df)
  }
