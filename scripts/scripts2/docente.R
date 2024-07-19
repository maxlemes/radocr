#
# ------------------- Função INTERNA - coleta  -------------------------------
#
#'Função que lê os dados dos RADOC e cria um dataframe com os dados do
#'docente
#'
#' @param pdf_file1  arquivo PDF com os dados do 1o RADOC
#' @param pdf_file2  arquivo PDF com os dados do 2o RADOC
#'
#' @return um dataframe com os dados do docente
#'
#' @examples
#' \dontrun{
#' docente(pdf_file1, pdf_file2)
#' }
#'
docente <- function(pdf_file1, pdf_file2) {

  #' Função que extrai os dados do Docente do arquivo PDF
  #' 
  #' @param pdf_file,  arquivo PDF com os dados do 1o RADOC
  #'
  #' @return um dataframe com os dados do docente
  #'
  dados_docente <- function(pdf_file) {

    # transformando o arquivo em tabela e depois em texto
    tabela <- readr::read_lines(pdftools::pdf_text(pdf_file))
    tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

    # Localizando os dados do docente na tabela
    ini <- xts::first(which(stringr::str_detect(tabela, "^Nome")))
    end <- xts::first(which(stringr::str_detect(tabela, "^Cargo")))

    df <- tabela[ini:end]

    df <- radocr:::tabtibble(df)

    # as vezes o nome da lotação passa para outra linha
    if (is.na(df[[2]][8])) {
      df[[4]][7] <- paste(df[[4]][7], df[[1]][8])
      df[[4]][9] <- df[[4]][7]
      df <- df[-8, ]
    }

    df <- df[-3, ]

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

    df <- radocr:::tabtibble(df)

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
        if (datas_fim[i] > reveillon) {
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
      df[2, 1] <- "Dias afastado"
      df[3, 1] <- "Meses afastado"
      df[, 2] <- as.numeric(NA)
      df[1, 2] <- n
      df[2, 2] <- total_dias
      df[3, 2] <- floor(total_dias / 30)
    } else {
      df <- tibble::tibble(
        "a" = c(
          "Afastamentos",
          "Dias afastado",
          "Meses afastado"
        ),
        "b" = c(0, 0, 0)
      )
    }

    colnames(df)[2] <- ano

    return(df)
  }

  #
  # --------------------  CORPO da função - docentes  --------------------------
  #
  # Coletando os dados do docente
  doc <- dados_docente(pdf_file1)

  # Verificando os afastamentos
  afast <- cbind(
    afastamentos(pdf_file1),
    afastamentos(pdf_file2)[, 2]
  )
  afast[, "Total"] <- rowSums(afast[, 2:3])

  # Juntando os afastamentos aos dados do docente
  if (afast[["Total"]][1] != 0) {
    aux <- afast[1:2, c(1, 4)]
    colnames(aux) <- colnames(doc)
    doc <- rbind(doc, aux)
  } else {
    aux <- tibble::tibble(
      "a" = "Afastamentos",
      "b" = "Nenhum Registro"
    )
    doc <- rbind(doc, aux)
  }

  return(doc)
}