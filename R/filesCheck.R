filesCheck <- function(file1, file2){
  #' Função que faz 3 testes nos arquivos
  #'
  #' 1) Testa se o arquivo é um RADOC do SICAD+
  #' 2) Testa se os dois RADOCs são do memso docente
  #' 3) Testa se os dois RADOCs são de anos distintos
  #'
  #' @param file1,file2 dois arquivos em pdf
  #'
  #' @return Uma mensagem com a avaliação feita
  #'
  #' @examples filesCheck(file1, file2)
  #'

  # file1 <- arquivos[1]
  # file2 <- arquivos[2]

  # Teste para verificar se os arquivos são RADOCs do SICAD+
  for (file in c(file1, file2)) {

    tabela <- readr::read_lines(pdftools::pdf_text(file))
    tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

    if (!grepl('^Sistema de Consulta das Atividades Docente', tabela[1])){
      stop(paste('O arquivo', file, 'não é um RADOC do SICAD+'))
    }
  }

  tabela1 <- readr::read_lines(pdftools::pdf_text(file1))
  tabela1 <- stringr::str_replace_all(tabela1, "^\\s+|\\s+$", "")
  tabela2 <- readr::read_lines(pdftools::pdf_text(file2))
  tabela2 <- stringr::str_replace_all(tabela2, "^\\s+|\\s+$", "")

  # Teste para verificar se os doir RADOCs são do mesmo docente
  aux1 <-  stringr::str_extract(
    tabela1[xts::first(which(stringr::str_detect(tabela1, "^Nome")))],
    '(^.*[A-Z]?[^\\s](?=\\s\\s))')
  aux2 <-  stringr::str_extract(
    tabela2[xts::first(which(stringr::str_detect(tabela2, "^Nome")))],
    '(^.*[A-Z]?[^\\s](?=\\s\\s))')

  if (aux1 != aux2){
    stop('Os RADOCs não são do mesmo docente')
  }

  # extraindo os anos dos Radocs
  ind_ano <- tabela1[xts::first(which(stringr::str_detect(tabela1, "^Relatório do docente")))]
  ano1 <- as.numeric(stringr::str_sub(
    stringr::str_extract(ind_ano, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),start = 5))

  ind_ano <- tabela2[xts::first(which(stringr::str_detect(tabela2, "^Relatório do docente")))]
  ano2 <- as.numeric(stringr::str_sub(
    stringr::str_extract(ind_ano, "(?<=\\()([^()]*?)(?=\\)[^()]*$)"),start = 5))

  if (ano1 == ano2){
    stop('Os RADOCs são do mesmo ano')
  }

  print('Os arquivos correspondem a 2 RADOCs válidos')
}
