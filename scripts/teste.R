

load('data/cad.RData')

for (j in c(1:length(radoc))){
  table <- pdf_text(radoc) %>%
    readr::read_lines()

table <- str_replace_all(table, ":", "|")
table <- str_replace_all(table,  "\\s{2,}", "|")
table[1] <- "1|2|3|4|5|6|7|8|9|10|11|12|13"

write(table, file=paste0("data/radoc",j,".txt"))

dt <- read_delim(paste0("data/radoc",j,".txt"),
                 delim = "|",
                 show_col_types = FALSE)

dt <- read_delim(paste0(table),
                 delim = "|",
                 show_col_types = FALSE)
}

strips <- "^\\s+|\\s+$"
for (i in 1:ncol(dt)){
  dt[[i]] <- gsub(strips,"",dt[[i]])
}


# extraindo o ano do Radoc
ano <- dt[3,2]
ano <- str_extract(ano, "(?<=\\()([^()]*?)(?=\\)[^()]*$)")
ano <- str_sub(ano,start = 5)

# resumo SICAD+ ----------------------------------------------------------------
df <- dt[15:16,]

aux <- tibble(
  'Categoria' = t(df[1,-ncol(df)]),
  ano = as.numeric(t(df[2,-1]))
) %>%
  rename(!! paste0(ano) := 'ano')


# salvando o SICAD+ ------------------------------------------------------------
sicad <- aux
save(sicad, file = paste0('data/sicadPlus_', ano, '.RData'))

# salvando o RADOC--------------------------------------------------------------
dt <- dt[,1:4]
colnames(dt) <- c('description', 'amount', 'time', 'points')
save(dt, file = 'data/radoc.RData')

# Atualizando a tabela da CAD --------------------------------------------------
aux <- tibble(
  'Dados' = cad[[1]],
  ano = as.numeric('')
)%>%
  rename(!! paste0(ano) := 'ano')

# salvando a tabela da CAD -----------------------------------------------------
cad <- bind_cols(cad,aux[-1])
save(cad, file = 'data/cad.RData')
