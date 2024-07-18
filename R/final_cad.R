#´ Função INTERNA que junta os dados dos RADOC, cria  e retorna 
#' umn dataframe com a Tabela da CAD dos dados 
#' 
#' @param pdf_file1,  arquivo PDF com os dados do 1o RADOC
#' @param pdf_file2,  arquivo PDFcom os dados do 2o RADOC
#' @param n,  carga horária do docende (padrão n=40)
#'
#' @return um dataframe com a Tabela CAD preenchida
#'
#' @examples
#' \dontrun{
#' final_cad(pdf_file1, pdf_file2, n=(20 ou 40))
#' }

final_cad <- function(pdf_file1, pdf_file2, n=40) {
  # capturando a tabela vazia
  cad <-  radocr::cad_orig

  files <- c(file1, file2)

  # montando o dataframe com os dados
  for(file in files){
    df <- radocr:::pontua(file)

    cad[, colnames(df)[4]] <- as.numeric(NA)

    lista_itens <- cad[[1]][1:21]

    for(item in lista_itens) {
      itens <- which(stringr::str_detect(df[[ 1 ]], paste0('^',item,'-')))
      dc <- df[itens,]
      cad[cad[[1]]== item,ncol(cad)] <- sum(dc[[ 4 ]])
    }
  }

  # ordenando os anos (caso os arquivos sejam inserido em ordem errada)
  anos <- as.numeric(colnames(cad)[3:ncol(cad)])
  ordem <- order(anos)
  cad[, 3:ncol(cad)] <- cad[, ordem + 2]

  # somando as pontuacoes dos anos analisados
  cad[, 'Total'] <- rowSums(cad[, 3:ncol(cad)])


  # calculando os valores de P e S
  cad[cad[[1]]=='P', ncol(cad)] <- sum(cad[cad[[1]] %in% c('I', 'II', "III", "IV", 'V'), ncol(cad)])
  cad[cad[[1]]=='S', ncol(cad)] <- sum(cad[cad[[1]] %in% c('I', "III", "IV", 'V'), ncol(cad)])

  # calculando a nota da CAD
  if (n == 40) {
    cad[cad[[1]]=='NF', ncol(cad)] <- min(cad[cad[[1]]=='P', ncol(cad)]/32, 10)
  } else {
    cad[cad[[1]]=='NF', ncol(cad)] <- min(cad[cad[[1]]=='P', ncol(cad)]/20, 10)
  }

  # ajustes finais
  for (i in 3:ncol(cad)){
    cad[[i]] <- as.character(format(cad[[i]], nsmall = 1))
    cad[[i]] <- stringr::str_replace_all(cad[[i]], "^\\s+|\\s+$", "")
    cad[[i]] <- sub(".", ",", cad[[i]], fixed = TRUE)
  }

  cad[cad[[1]] %in% c('P','NF','S'), 3:4] <- '-'
 
  return(cad)
}