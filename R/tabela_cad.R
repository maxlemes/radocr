tabela_cad <- function (file1, file2, n=40){
  #' Retorna o dataframe pronto para impressão da Tabela Cad
  #'
  #' @param df dataframe com os dados do RADOC
  #'
  #' @return um dataframe com a Tabela CAD preenchida
  #'
  #' @examples
  #' \dontrun{
  #' tabela_cad(df)
  #' }
  #' 
  
  filesCheck(file1,file2)



  # capturando a tabela vazia
  cad <- cad_orig

  files <- c(file1, file2)
 
  # montando o dataframe com os dados
  for(file in files){
    df <- pontua(file)

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


  # Coletando os dados do docente
  doc <- docente(file1)

  # Verificando os afastamentos  
  afast <- cbind(afastamentos(file1), afastamentos(file2)[,2])
  afast[, 'Total'] <- rowSums(afast[, 2:3])

  if (afast[['Total']][1] != 0) {
    aux <- afast[1:2,c(1,4)]
    colnames(aux) <- colnames(doc)
    doc <- rbind(doc, aux)
  } else {
    aux <- tibble::tibble(
      'a' = 'Afastamentos',
      'b' = 'Nenhum Registro'
    )
    doc <- rbind(doc, aux)
  }

  # gerando uma tabela em LaTeX com os dados do docente
  # transforma em uma tabela de latex
  df <- xtable::xtable(doc)

  # alinhamento das colunas
  xtable::align(df) <- c(rep('l',2),'l')

  # gerando o arquivo com a tabela para LaTeX
  print(df,
      scalebox = 1.1,
      hline.after = c(-1,nrow(df)),
      sanitize.text.function=function(x){x},
      include.rownames=FALSE,
      include.colnames=FALSE,
      file = "tabelas/docente.tex")
  
  # gerando a tabela da CAD em Latex
  # transforma em uma tabela de latex
  aux <- cad
  for (i in 3:ncol(aux)){
    aux[[i]] <- as.character(format(aux[[i]], nsmall = 1))
    aux[[i]] <- stringr::str_replace_all(aux[[i]], "^\\s+|\\s+$", "")
  }
  aux[aux[[1]] %in% c('P','NF','S'), 3:4] <- '-'

  df <- xtable::xtable(aux)
  
  # alinhamento das colunas
  xtable::align(df) <- c('lll|r|r|r')
  
  # colocando linhas em negrito
  itens  <- c(1,5,10,13,18,22,23,24)
  for (j in itens){
    df[j,] = paste0('\\textbf{',df[j,],'}')
  }

  # criando o arquivo com a tabela
  bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
  gray <- function(x) {paste('{\\textcolor{gray}{',x,'}}', sep ='')}

  # Definindo onde as linhas verticais serão adicionadas  
  print(df,
        scalebox = 0.9,
        hline.after = c(-1,0, nrow(df)),
        include.rownames=FALSE,
        include.colnames=TRUE,
        sanitize.text.function=function(x){x},
        sanitize.rownames.function=gray,
        sanitize.colnames.function=bold,
        file = "tabelas/notas_cad.tex")
  
  
  # Saida em Excel

  doc[(nrow(doc)+1),] <- as.list(c('', ''))
  doc[(nrow(doc)+1),] <- as.list(c('', ''))
  doc[, 'c'] <- ''
  doc[, 'd'] <- ''
  doc[, 'e'] <- ''

  aux <- aux[c(1,1:nrow(aux)),]
  aux[1,] <- as.list(colnames(aux))
  colnames(aux) <- colnames(doc)

  aux <- rbind(doc, aux)

  writexl::write_xlsx(aux, path = 'tabelas/tabela_cad.xlsx')
}
