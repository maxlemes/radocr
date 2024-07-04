
load('data/tabela.RData')
load('data/cad.RData')


# extraindo o ano do Radoc
ano <- str_extract(tabela[8], "(?<=\\()([^()]*?)(?=\\)[^()]*$)")
ano <- str_sub(ano,start = 5)
save(ano, file = paste0("data/ano.RData"))


# Localizando os dados do SICAD+ na tabela -------------------------------------
ini <- tabela %>% str_detect("^Carga Horária") %>% which
ini <- ini[1] + 1
end <- ini + 1

df <- tabela[ini:end]

df <- df %>%
  str_replace_all(.,  "\\s{2,}", "|") %>%
  str_replace_all(., "^\\s+|\\s+$", "|") %>%
  str_replace_all(.,  "^\\|", "")

df <-df %>%
  as_tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:12],
                       cols_remove = TRUE)

df <- tibble(
  'Categoria' = as.character(t(df[1,])),
  ano = as.numeric(t(df[2,]))
) %>%
  rename(!! paste0(ano) := 'ano')

# salvando o SICAD+ ----------------------------------------
sicad <- df
save(sicad, file = paste0('data/sicadPlus_', ano, '.RData'))
# ------------------------------------------------------------------------------

df <- tabela

ini <- df %>% str_detect("^Afastamentos") %>% which

df <- df[ini:length(tabela)]

df <- df %>%
  str_replace_all(., ":", "|") %>%
  str_replace_all(.,  "\\s{2,}", "|") %>%
  str_replace_all(., "^\\s+|\\s+$", "|") %>%
  str_replace_all(.,  "^\\|", "")

dt <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

# salvando o RADOC------------------------------------------
dt <- dt[,1:4]
colnames(dt) <- c('description', 'amount', 'time', 'points')
save(dt, file = 'data/radoc.RData')


# Atualizando a tabela da CAD --------------------------------------------------
cad[, ano] <- as.numeric('')

# salvando a tabela da CAD -----------------------------------------------------
save(cad, file = 'data/cad.RData')
