
library(dplyr)
library(stringr)
library(magrittr)


cad <- paste0('data-raw/','Resolucao_tab.pdf')

df <- pdftools::pdf_text(cad) %>% readr::read_lines()

df <- df %>%
  str_replace_all(., ":", "|") %>%
  str_replace_all(.,  "\\s{2,}", "|")%>%
  str_replace_all(., "^\\s+|\\s+$", "|")%>%
  str_replace_all(.,  "^\\|", "")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:4],
                       too_few = "align_start",
                       cols_remove = TRUE)

df <- df[df[[1]]!='',c(1:2)]


# df[is.na(df)] <- ''
# df[[1]] <- paste(df[[1]], df[[2]], sep = ' - ')

end <- df[[1]]%>% str_detect("^I-1") %>% which - 2
df <- df[-c(1:end),1:2]

itens <- df[[1]]%>% str_detect("^Pontuação total do item") %>% which
df <- df[-itens,1:2]

itens <- df[[1]]%>% str_detect("^2") %>% which
df <- df[-itens,1:2]

end <- df[[1]]%>% str_detect("^P ± soma") %>% which - 1
df <- df[c(1:end),1:2]

dt <- tibble(
  'a' = c('P',
          'NF',
          'S'),
  'b'  = c('I + II + III + IV + V',
           'NOTA FINAL DA CAD', # 40h: min(10 e P/32) ou 20H: min(10 e P/20)
           'I + III + IV + V'
  )
)

df <- rbind(df, dt)

colnames(df) <- c('Item', 'Avaliação de desempenho')

I <- df[[1]]%>% str_detect("^I") %>% which %>% first
II <- df[[1]]%>% str_detect("^II") %>% which %>% first
III <- df[[1]]%>% str_detect("^III") %>% which %>% first
IV <- df[[1]]%>% str_detect("^IV") %>% which %>% first
V <- df[[1]]%>% str_detect("^V") %>% which %>% first

df[[2]][I] <- 'ATIVIDADES DE ENSINO'
df[[2]][II] <- 'PRODUÇÃO INTELECTUAL'
df[[2]][III] <- 'ATIVIDADES DE PESQUISA E DE EXTENSÃO'
df[[2]][IV] <- 'ATIVIDADES ADMINISTRATIVAS E DE REPRESENTAÇÃO'
df[[2]][V] <- 'OUTRAS ATIVIDADES'

cadOrig <- df
save(cad, file = 'data/cadOrig.rda')
