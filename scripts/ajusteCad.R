library(xtable)

load('data/cad.RData')
load('data/docente.RData')
load('data/processo.RData')

# Dados do docente -------------------------------------------------------------
df <- docente

# selecionando os dados essencias------------------------------------
df <- tibble(
  'a' = c(
    paste0('\\textbf{', str_extract(df[[1]][1], "(^.*[A-z]?:)"), '}'),
    paste0('\\textbf{', str_extract(df[[2]][1], "(^.*[A-z]?:)"), '}'),
    paste0('\\textbf{', str_extract(df[[1]][2], "(^.*[A-z]?:)"), '}'),
    paste0('\\textbf{', str_extract(df[[2]][2], "(^.*[A-z]?:)"), '}'),
    '\\textbf{Processo:}',
    '\\textbf{RADOCs avaliados:}'
  ),
  'b' = c(
    str_extract(df[[1]][1], "(?<=: ).*$"),
    str_extract(df[[2]][1], "(?<=: ).*$"),
    str_extract(df[[1]][2], "(?<=: ).*$"),
    str_extract(df[[2]][2], "(?<=: ).*$"),
    processo,
    paste(colnames(cad)[3], 'e', colnames(cad)[4])
  )
)

# transforma em uma tabela de latex
df <- xtable(df)

# alinhamento das colunas
align(df) <- c(rep('l',2),'l')

# gerando o arquivo para LaTeX
print(df,
      scalebox = 1.1,
      hline.after = c(-1,nrow(df)),
      sanitize.text.function=function(x){x},
      include.rownames=FALSE,
      include.colnames=FALSE,
      file = "texFiles/docente.tex")

# Tabela de Avaliação da CAD ---------------------------------------------------

df <- cad

df[,'Pontuação'] <- df[[3]] + df[[4]]

# salvando a tabela da CAD ----------
cad <- df
save(cad, file = "data/cad.RData")


# encontrando os itens  essenciais
I <- df[[1]]%>% str_detect("^I") %>% which %>% first
II <- df[[1]]%>% str_detect("^II") %>% which %>% first
III <- df[[1]]%>% str_detect("^III") %>% which %>% first
IV <- df[[1]]%>% str_detect("^IV") %>% which %>% first
V <- df[[1]]%>% str_detect("^V") %>% which %>% first
P <- df[[1]]%>% str_detect("^P") %>% which
NF <- df[[1]]%>% str_detect("^NF") %>% which
S <- df[[1]]%>% str_detect("^S") %>% which

# excluindo as colunas 3 e 4
df <- df[,-c(3,4)]

# Calculando a nota da CAD
df[[ncol(df)]][NF] <- round(min(10,df[[ncol(df)]][P]/32),2)

# colocando 1 casa de arredondamento
df[[ncol(df)]]  <- format(round(df[[ncol(df)]], 1), nsmall = 1)


itens  <- c(I, II, III, IV, V)

# transforma em uma tabela de latex
df <- xtable(df)

# alinhamento das colunas
align(df) <- c('ll|l|r')

# colocando linhas em negrito
itens  <- c(I, II, III, IV, V, P, NF, S)
for (j in itens){
  df[j,] = paste0('\\textbf{',df[j,],'}')
}

situacao <- 'APROVADO'

# criando o arquivo com a tabela
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
gray <- function(x) {paste('{\\textcolor{gray}{',x,'}}', sep ='')}

n <- nrow(df)
addtorow <- list()
addtorow$pos <- list(n)
addtorow$command <- c(
  paste('& \\textbf{RESULTADO DA AVALIAÇÃO} &  \\textbf{',situacao, '} \\\\\n'))
print(df,
      scalebox = 1,
      booktabs = T,
      hline.after = c(0,nrow(df)),
      add.to.row = addtorow,
      include.rownames=FALSE,
      include.colnames=TRUE,
      sanitize.text.function=function(x){x},
      sanitize.rownames.function=gray,
      sanitize.colnames.function=bold,
      type = "latex",
      file = "texFiles/notasCad.tex")


