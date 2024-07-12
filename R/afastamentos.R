afastamentos <- function(file) {
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

  df <- tabela[ini:(end - 1)]

  df <- tabtibble(df)

  # Quando há afastamento
  if (df[[1]][2] != "Nenhum registro.") {

    # definindo o inicio e o fim do ano corrente
    newyear <- as.Date(paste0('01/01/',ano), format = '%d/%m/%Y')
    reveillon <- as.Date(paste0('31/12/',ano), format = '%d/%m/%Y')


    # Eoncontrando as das iniciais dos afastamentos
    ini <- which(grepl("^Data de In\u00edcio", df[[1]]))

    datas_inicio <- lapply(df[ini,2],
                    FUN = function(x){as.Date(x, format = '%d/%m/%Y')})[[1]]

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

    datas_fim <- lapply(df[end,2],
                      FUN = function(x){as.Date(x, format = '%d/%m/%Y')})[[1]]

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
    df <- df[1:3,1:2]
    df[2,1] <- 'Dias afastado'
    df[3,1] <- 'Meses afastado'
    df[,2]  <- as.numeric(NA)
    df[1,2] <- n
    df[2,2] <- total_dias
    df[3,2] <- floor(total_dias/30)
  } else  {
    df <- tibble::tibble(
      'a' = c('Afastamentos',
              'Dias afastado',
              'Meses afastado'),
      'b' = c(0,0,0))
  }

  colnames(df)[2] <- ano

  return(df)
}

