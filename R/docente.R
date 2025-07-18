#
# ------------------- Função INTERNA - docente  -------------------------------
#
#' Extrai e organiza dados do docente
#' 
#' @description
#' A função `docente()` lê os dados dos RADOC e cria um dataframe contendo as 
#' informações do docente, incluindo os afastamentos no período.
#' 
#' @details
#' Primeiramente, a função lê os arquivos PDF e extrai os dados pessoais do 
#' docente. Em seguida, ela extrai os dados de afastamento e, por último, junta 
#' todas as informações em um dataframe.
#'
#' @param pdf_file1 Um arquivo PDF contendo os dados do 1º RADOC.
#' @param pdf_file2 Um arquivo PDF contendo os dados do 2º RADOC.
#'
#' @return Um dataframe com os dados do docente, incluindo informações de 
#' afastamento.
#'
#' @examples
#' \dontrun{
#' # Exemplo de uso da função docente
#' docente(pdf_file1, pdf_file2)
#' }
#' @keywords internal
docente <- function(...) {

  pdf_files <- c(...)

  # Coletando os dados do docente
  doc <- dados_docente(pdf_files[1])

  # Verificando os afastamentos
  afast <- afastamentos(pdf_files[1])

  if (length(pdf_files) >1){
    for (j in 2:length(pdf_files)){
      afast <- cbind(
        afast,
        afastamentos(pdf_files[j])[, 2]
      )
    }
    afast[, "Total"] <- rowSums(afast[, 2:ncol(afast)])

    # Juntando os afastamentos aos dados do docente
    if (afast[["Total"]][1] != 0) {
      aux <- afast[1:3, c(1, ncol(afast))]
      colnames(aux) <- colnames(doc)
      doc <- rbind(doc, aux)
    } else {
      aux <- tibble::tibble(
        "a" = "Afastamentos",
        "b" = "Nenhum Registro"
      )
      doc <- rbind(doc, aux)
    }
  } else {
    if (afast[[2]][2] != 0){
      aux <- afast[1:3, ]
      colnames(aux) <- colnames(doc)
      doc <- rbind(doc, aux)
    } else {
      aux <- tibble::tibble(
        "a" = "Afastamentos",
        "b" = "Nenhum Registro"
      )
      doc <- rbind(doc, aux)
    }
  }

  return(doc)
}
#
#---------------------Função INTERNA - dados_docente  --------------------------
#
#' Extrai os dados do Docente do arquivo PDF
#' 
#' @param pdf_file  arquivo PDF com os dados do RADOC
#' 
#' @return um dataframe com os dados do docente
#' 
#' @examples
#' \dontrun{
#' dados_docente(pdf_file)
#' }
#' @keywords internal
dados_docente <- function(pdf_file) {

  # transformando o arquivo em tabela e depois em texto
  tabela <- readr::read_lines(pdftools::pdf_text(pdf_file))
  tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

  # Localizando os dados do docente na tabela
  ini <- xts::first(which(stringr::str_detect(tabela, "^Nome")))
  end <- xts::first(which(stringr::str_detect(tabela, "^Cargo")))

  df <- tabela[ini:end]

  df <- tabtibble(df)

  # as vezes o nome da lotação passa para outra linha
  if (length(df)==8){
    if (is.na(df[[2]][8])) {
      df[[4]][7] <- paste(df[[4]][7], df[[1]][8])
      df[[4]][9] <- df[[4]][7]
      df <- df[-8, ]
    }
  }

  print(df)

  for (i in 1:length(df)){
    if (df[[i]][[1]]=='Dados Funcionais'){
      df <- df[i, ]
    }
  }
  print(df)


  # ajustes
  df[[1]][1] <- "Docente"
  df[[1]][3] <- "Matr\u00edcula SIAPE"
  df[[3]][6] <- "Unidade"
  df[[4]][3] <- paste(df[[4]][3], "- N\u00edvel", df[[4]][4])
  df[[3]][4] <- df[[1]][7]
  df[[4]][4] <- df[[2]][7]
  df <- df[-7, ]

  # Filtrando só o que interessa
  df[1, 3:4] <- df[2, 3:4]
  df[4, 3:4] <- df[5, 3:4]
  df <- df[-c(2, 5), ]

  # empilhando em 2 colunas
  aux <- df[, 3:4]
  colnames(aux) <- colnames(df[, 1:2])
  df <- rbind(df[, 1:2], aux)

  df <- df[c(1, 5, 2, 3, 7, 4, 6, 8), ]
  df[5, 2] <- to_title_case_pt_br(df[5, 2])
  df[8, 2] <- to_title_case_pt_br(df[8, 2])

  return(df)
}
#
#---------------------Função INTERNA - afastamentos  ---------------------------
#
#' Função que extrai os dado de afastamentos do docente do RADOC em analise
#'
#' @param pdf_file arquivo em pdf do RADOC do docente
#'
#' @return um dataframe com os dados do afastamento do docente
#'
#' @examples
#' \dontrun{
#' afastamentos(pdf_file)
#' }
#' @keywords internal
afastamentos <- function(pdf_file) {

  # transformando o arquivo em tabela e depois em texto
  tabela <- readr::read_lines(pdftools::pdf_text(pdf_file))
  tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

  # extraindo o ano do Radoc
  ano <- tabela[xts::first(which(
    stringr::str_detect(tabela, "^Relat\u00f3rio do docente")
  ))]
  ano <- as.numeric(stringr::str_sub(
    stringr::str_extract(ano, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),
    start = 5
  ))

  # Localizando os dados do docente na tabela
  ini <- xts::first(which(stringr::str_detect(tabela, "^Afastamentos")))
  end <- xts::first(which(stringr::str_detect(tabela, "^I - ATIVIDADES DE ENSINO")))

  df <- tabela[ini:(end - 1)]

  df <- tabtibble(df)

  # Quando há afastamento
  if (df[[1]][2] != "Nenhum registro.") {
    # definindo o inicio e o fim do ano corrente
    newyear <- as.Date(paste0("01/01/", ano), format = "%d/%m/%Y")
    reveillon <- as.Date(paste0("31/12/", ano), format = "%d/%m/%Y")


    # Eoncontrando as das iniciais dos afastamentos
    ini <- which(grepl("^Data de In\u00edcio", df[[1]]))

    datas_inicio <- lapply(df[ini, 2],
      FUN = function(x) {
        as.Date(x, format = "%d/%m/%Y")
      }
    )[[1]]

    # Ordenar os intervalos pelas datas de início
    ordem <- order(datas_inicio)

    # considerando apenas datas no ano corrente
    for (i in ordem) {
      if (datas_inicio[i] < newyear) {
        datas_inicio[i] <- newyear
      }
    }

    # encontrando as datas finais
    end <- which(grepl("^Data de T\u00e9rmino", df[[1]]))

    datas_fim <- lapply(df[end, 2],
      FUN = function(x) {
        as.Date(x, format = "%d/%m/%Y")
      }
    )[[1]]

    # considerando apenas datas no ano corrente
    for (i in ordem) {
      if ((is.na(datas_fim[i]))|(datas_fim[i] > reveillon)) {
        datas_fim[i] <- reveillon
      }
    }

    # testando se exitem datas a serem analisadas
    n <- length(datas_inicio)
    if (n == 0) {
      return(0)
    }

    # Ordenar os intervalos pelas datas de início
    datas_inicio <- datas_inicio[ordem]
    datas_fim <- datas_fim[ordem]

    # Inicializar variáveis
    inicio <- datas_inicio[1]
    fim <- datas_fim[1]
    total_dias <- 0

    # Percorrer os intervalos
    if (n > 1) {
      for (i in 2:n) {
        if (datas_inicio[i] <= fim) {
          # Se o próximo intervalo se sobrepõe ao intervalo atual, combine-os
          fim <- max(fim, datas_fim[i])
        } else {
          # Caso contrário, calcular os dias do intervalo atual e reiniciar para o próximo
          total_dias <- total_dias +
            as.numeric(difftime(fim, inicio, units = "days")) + 1
          inicio <- datas_inicio[i]
          fim <- datas_fim[i]
        }
      }
    }

    # Adicionar o último intervalo calculado
    total_dias <- total_dias +
      as.numeric(difftime(fim, inicio, units = "days")) + 1

    # organizando a saida
    df <- df[1:3, 1:2]
    df[2, 1] <- "Dias de afastamento"
    df[3, 1] <- "Semanas de afastamento"
    df[, 2] <- as.numeric(NA)
    df[1, 2] <- n
    df[2, 2] <- total_dias
    df[3, 2] <- floor(total_dias / 7)
  } else {
    df <- tibble::tibble(
      "a" = c(
        "Afastamentos",
        "Dias de afastamento",
        "Semanas de afastamento"
      ),
      "b" = c(0, 0, 0)
    )
  }

  colnames(df)[2] <- ano

  return(df)
}