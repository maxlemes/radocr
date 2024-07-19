#' Extrai os dados do Docente
#'
#' @param pdf_file arquivo PDF do RADOC do docente
#'
#' @return um dataframe com os dados do docente
#'
#' @examples
#' \dontrun{
#' docente(tabela)
#' }
docente <- function (file){

  # transformando o arquivo em tabela e depois em texto
  tabela <- readr::read_lines(pdftools::pdf_text(file))
  tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

  # Localizando os dados do docente na tabela
  ini <- xts::first(which(stringr::str_detect(tabela, "^Nome")))
  end <- xts::first(which(stringr::str_detect(tabela, "^Cargo")))

  df <- tabela[ini:end]

  df <- radocr:::tabtibble(df)

  # as vezes o nome da lotação passa para outra linha
  if (is.na(df[[2]][8])){
    df[[4]][7] <- paste(df[[4]][7],df[[1]][8])
    df[[4]][9] <-  df[[4]][7]
    df <- df[-8,]
  }

  df <- df[-3,]

  # ajustes 
  df[[1]][1] <- 'Docente'
  df[[1]][3] <- 'Matr\u00edcula SIAPE'
  df[[3]][6] <- 'Unidade'
  df[[4]][3] <- paste(df[[4]][3], '- N\u00edvel', df[[4]][4])
  df[[3]][4] <- df[[1]][7]
  df[[4]][4] <- df[[2]][7]
  df <- df[-7,]

  # Filtrando só o que interessa
  df[1, 3:4] <- df[2, 3:4]
  df[4, 3:4] <- df[5, 3:4]
  df <- df[-c(2,5),]

  # empilhando em 2 colunas
  aux <- df[,3:4]
  colnames(aux) <- colnames(df[,1:2])
  df <- rbind(df[,1:2], aux)
  
  df <- df[c(1,5,2,3,7,4,6,8),]
  df[5,2] <- to_title_case_pt_br(df[5,2])
  df[8,2] <- to_title_case_pt_br(df[8,2])
  
  return(df)
}
