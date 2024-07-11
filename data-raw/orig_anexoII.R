rm(list = ls())

library(pdftools)
library(magrittr)
library(tidyverse)
library(stringr)

anexoII <- paste0('data-raw/Anexo_II.pdf')

tabela <- anexoII %>%
  pdftools::pdf_text() %>%
  readr::read_lines()

# I - ATIVIDADES DE ENSINO -----------------------------------------------------
df <- tabela

ini <- df %>% str_detect("^    I ± ATIVIDADES DE ENSINO*") %>% which
end <- df %>% str_detect("^    II - PRODUÇÃO INTELECTUAL") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|") %>%
  str_replace_all(., "^\\s+|\\s+$", "\\|") %>%
  str_replace_all(.,  "^\\|", "")

df <- df %>%
  as_tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens I-1-1 a I-2-2 -----------------------------------------------
da <- tibble(
  'Item' = c('I-1-1', 'I-1-2', 'I-2-1', 'I-2-2'),
  'Descrição' = c(
    'Disciplina de Ensino Básico ou Graduação - presencial',
    'Disciplina de Ensino Básico ou Graduação - a distância',
    'Disciplina de Pós-Graduação - presencial',
    'Disciplina de Pós-Graduação - a distância'
  ),
  'Pontos' = 10
)

df <- df[-c(1:20),]

# Itens I-3-1 e I-3-2 -----------------------------------------------
aux <- tibble(
  'Item' = c('I-3-1', 'I-3-2'),
  'Descrição' = c(
    'Coordenador de projeto de ensino com financiamento',
    'Coordenador de projeto de ensino sem financiamento'
  ),
  'Pontos' = as.numeric(df[[3]][c(3,5)])
)

da <- rbind(da, aux)

# II - PRODUÇÃO INTELECTUAL ----------------------------------------------------
# II -1 Produção Científica ----------------------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^    II - PRODUÇÃO INTELECTUAL") %>% which
end <- df %>% str_detect("^II - 2 Produção Artística e Cultural") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|") %>%
  str_replace_all(., "^\\s+|\\s+$", "\\|") %>%
  str_replace_all(.,  "^\\|", "")

df <-df %>%
  as_tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens II-1-1.1 a II-1-1.4 -----------------------------------------------
aux <- tibble(
  'Item' = c('II-1-1.1', 'II-1-1.2', 'II-1-1.3', 'II-1-1.4'),
  'Descrição' = c(
    'Artigo completo publicado em periódico - Qualis/CAPES A',
    'Artigo completo publicado em periódico - Qualis/CAPES B',
    'Artigo completo publicado em periódico - Qualis/CAPES C',
    'Artigo completo publicado em periódico - Sem Qualis/CAPES'
  ),
  'Pontos' = as.numeric(df[[3]][c(6,8,10,12)])
)

da <- rbind(da, aux)

df <- df[-c(1:12),]

# Itens II-1-2 e II-1-3 -----------------------------------------------
aux <- tibble(
  'Item' = c('II-1-2', 'II-1-3'),
  'Descrição' = c(
    'Resumo de artigo em periódicos especializados com corpo editorial',
    'Artigos ou textos literários em repositórios de publicação eletrônica'
    ),
  'Pontos' = as.numeric(df[[3]][c(3,5)])
)

da <- rbind(da, aux)

df <- df[-c(1:5),]

# Itens II-1-4.1 a II-1-4.3 -----------------------------------------------
aux <- tibble(
  'Item' = c('II-1-4.1', 'II-1-4.2', 'II-1-4.3'),
  'Descrição' = c(
    'Resumo expandido publicado em anais de congresso - Internacional',
    'Resumo expandido publicado em anais de congresso - Nacional',
    'Resumo expandido publicado em anais de congresso - Regional ou Local'
  ),
  'Pontos' = as.numeric(df[[3]][c(6,8,10)])
)

da <- rbind(da, aux)

df <- df[-c(1:10),]

# Itens II-1-5.1 a II-1-5.3 -----------------------------------------------
aux <- tibble(
  'Item' = c('II-1-5.1', 'II-1-5.2', 'II-1-5.3'),
  'Descrição' = c(
    'Resumo simples publicado em anais de congresso - Internacional',
    'Resumo simples publicado em anais de congresso - Nacional',
    'Resumo simples publicado em anais de congresso - Regional ou Local'
  ),
  'Pontos' = as.numeric(df[[3]][c(8,10,12)])
)

da <- rbind(da, aux)

df <- df[-c(1:12),]

# Itens II-1-6 até  II-1-16 -----------------------------------------------
aux <- tibble(
  'Item' = c('II-1-6', 'II-1-7', 'II-1-8','II-1-9', 'II-1-10', 'II-1-11',
            'II-1-12', 'II-1-13', 'II-1-14','II-1-15', 'II-1-16'),
  'Descrição' = c(
    'Trabalho completo publicado em anais de congresso científico',
    'Livro publicado com selo de editora com corpo editorial',
    'Livro publicado com selo de editora sem corpo editorial',
    'Capítulo de livro publicado com selo de editora com corpo editorial',
    'Edição ou organização de livro publicado em editora com corpo editorial',
    'Capítulo traduzido de livro publicado em editora com corpo editorial',
    'Tradução de livro publicado em editora com corpo editorial',
    'Tradução de artigos publicados em periódicos com classificação no Qualis',
    'Resenhas, prefácios ou verbetes',
    'Tradução de resenhas, prefácios ou verbetes',
    'Livro didático desenvolvido para projetos institucionais/governamentais'
  ),
  'Pontos' = as.numeric(df[[3]][c(2,4,6,8,11,14,17,19,21,23,25)])
)

da <- rbind(da, aux)

df <- df[-c(1:25),]

# Itens II-1-17.1 a II-1-17.3 -----------------------------------------------
aux <- tibble(
  'Item' = c('II-1-17.1', 'II-1-17.2', 'II-1-17.3'),
  'Descrição' = c(
    'Editor de Anais de Eventos - Internacional',
    'Editor de Anais de Eventos - Nacional',
    'Editor de Anais de Eventos - Regional ou Local'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6,8)])
)

da <- rbind(da, aux)

df <- df[-c(1:8),]

# Itens II-1-18 a II-1-20 -------------------------------------------
aux <- tibble(
  'Item' = c('II-1-18', 'II-1-19', 'II-1-20'),
  'Descrição' = c(
    'Dissertação de Mestrado defendida e aprovada',
    'Tese de Doutorado defendida e aprovada',
    'Bolsista de Produtividade do CNPq'
  ),
  'Pontos' = as.numeric(df[[3]][c(2,4,6)])
)

da <- rbind(da, aux)

# II-2 Produção Artística e Cultural -------------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^II - 2 Produção Artística e Cultural") %>% which
end <- df %>% str_detect("^II - 3 Produção Técnica e Tecnológica") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|") %>%
  str_replace_all(., "^\\s+|\\s+$", "\\|") %>%
  str_replace_all(.,  "^\\|", "")

df <-df %>%
  as_tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens II-2-1 a II-12-1.3 -------------------------------------------
aux <- tibble(
  'Item' = c('II-2-1.1', 'II-2-1.2', 'II-2-1.3'),
  'Descrição' = c(
    'Criação, produção e direção de filmes, vídeos, etc. - Locais ou regionais',
    'Criação, produção e direção de filmes, vídeos, etc. - Nacionais',
    'Criação, produção e direção de filmes, vídeos, etc. - Internacionais'
  ),
  'Pontos' = as.numeric(df[[3]][c(6,8,10)])
)

da <- rbind(da, aux)

df <- df[-c(1:10),]

# Itens II-2-2 a II-2-5 -------------------------------------------
aux <- tibble(
  'Item' = c('II-2-2', 'II-2-3', 'II-2-4', 'II-2-5'),
  'Descrição' = c(
    'Criação e produção do projeto gráfico de livros',
    'Criação de trilha sonora para cinema, televisão ou teatro',
    'Criação e produção de projeto de iluminação, figurinos, etc.',
    'Design de impressos por peça'
  ),
  'Pontos' = c(10,15,15,1) #------------------------------------
)

da <- rbind(da, aux)

df <- df[-c(1:9),]

# Itens II-2-6  a II-2-6.2 -------------------------------------------
aux <- tibble(
  'Item' = c('II-2-6.1', 'II-2-6.2'),
  'Descrição' = c(
    'Exposições artísticas locais ou regionais - Participação individual',
    'Exposições artísticas locais ou regionais - Participação coletiva'
    ),
  'Pontos' = as.numeric(df[[3]][c(4,6)])
)

da <- rbind(da, aux)

df <- df[-c(1:6),]

# Itens II-2-7 a II-2-2 -------------------------------------------
aux <- tibble(
  'Item' = c('II-2-7.1', 'II-2-7.2'),
  'Descrição' = c(
    'Exposições artísticas nacionais - Participação individual',
    'Exposições artísticas nacionais - Participação coletiva'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6)])
)

da <- rbind(da, aux)

df <- df[-c(1:6),]

# Itens II-2-8.1 a II-2-8.2-------------------------------------------
aux <- tibble(
  'Item' = c('II-2-8.1', 'II-2-8.2'),
  'Descrição' = c(
    'Exposições artísticas internacionais - Participação individual',
    'Exposições artísticas internacionais - Participação coletiva'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6)])
)

da <- rbind(da, aux)

df <- df[-c(1:6),]

# Itens II-2-9.1 a II-2-9.4 -------------------------------------------
aux <- tibble(
  'Item' = c('II-2-9.1', 'II-2-9.2', 'II-2-9.3', 'II-2-9.4'),
  'Descrição' = c(
    'Composições musicais - Editadas',
    'Composições musicais - Publicadas em revistas científicas',
    'Composições musicais - Gravadas',
    'Composições musicais - Executadas em apresentações públicas'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6,8,10)])
)

da <- rbind(da, aux)

df <- df[-c(1:10),]

# Itens II-2-10.1 a II-2-10.3 -------------------------------------------
aux <- tibble(
  'Item' = c('II-2-10.1', 'II-2-10.2', 'II-2-10.3'),
  'Descrição' = c(
    'Produção premiada em evento - Local ou regional',
    'Produção premiada em evento - Nacional',
    'Produção premiada em evento - Internacional'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6,8)])
)

da <- rbind(da, aux)

df <- df[-c(1:8),]

# Itens II-2-11 a II-2-14 -------------------------------------------
aux <- tibble(
  'Item' = c('II-2-11', 'II-2-12', 'II-2-13', 'II-2-14'),
  'Descrição' = c(
    'Arranjos musicais (canto, coral e orquestral)',
    'Apresentação artística ou cultural em rádio ou TV',
    'Sonoplastia (cinema, música, rádio, televisão, teatro)',
    'Fotos publicitárias, jornalísticas, etc.'
  ),
  'Pontos' = as.numeric(df[[3]][c(2,4,6,8)])
)

da <- rbind(da, aux)

# II-3 Produção Técnica e Tecnológica ------------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^II - 3 Produção Técnica e Tecnológica") %>% which
end <- df %>% str_detect("^II - 4 Outro Tipo de Produção") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens II-3-1 a II-3-12 -------------------------------------------
aux <- tibble(
  'Item' = c('II-3-1', 'II-3-2', 'II-3-3','II-3-4', 'II-3-5', 'II-3-6',
            'II-3-7', 'II-3-8', 'II-3-9','II-3-10', 'II-3-11'),
  'Descrição' = c(
    'Desenvolvimento de software com registro livre',
    'Desenvolvimento de software com divulgação em periódicos',
    'Desenvolvimento de software para uso institucional',
    'Desenvolvimento e registro no INPI de topografia de circuito integrado',
    'Desenvolvimento de produto, processo ou técnica com registro de patente',
    'Desenvolvimento e registro no INPI de desenho industrial',
    'Desenvolvimento e registro no INPI de processo de indicação geográfica',
    'Desenvolvimento e registro no INPI de marcas',
    'Membro de corpo editorial de periódicos com classificação Qualis',
    'Parecer ad hoc de livros para publicação em editoras com corpo editorial',
    'Parecer ad hoc artigos para publicação em periódicos com corpo editorial'
  ),
  'Pontos' = as.numeric(df[[3]][c(3,6,9,12,14,17,19,21,23,26,29)])
)

da <- rbind(da, aux)

df <- df[-c(1:29),]

# Itens II-3-12.1 a II-3-12.3 -------------------------------------------
aux <- tibble(
  'Item' = c('II-3-12.1', 'II-3-12.2', 'II-3-12.3'),
  'Descrição' = c(
    'Parecer ad hoc de trabalhos para eventos - Trabalho completo',
    'Parecer ad hoc de trabalhos para eventos - Resumo expandido',
    'Parecer ad hoc de trabalhos para eventos - Resumo'
  ),
  'Pontos' = as.numeric(df[[3]][c(6,8,10)])
)

da <- rbind(da, aux)

df <- df[-c(1:10),]

# Itens II-3-13 e II-3-14 -------------------------------------------
aux <- tibble(
  'Item' = c('II-3-13', 'II-3-14'),
  'Descrição' = c(
    'Revisão ad hoc de: periódico com Qualis ou livro',
    'Coordenação de mesas redondas, simpósios ou sessões de comunicações'
  ),
  'Pontos' = as.numeric(df[[3]][c(2,6)])
)

da <- rbind(da, aux)

df <- df[-c(1:6),]

# Itens II-3-15.1 a II-3-15.4 -------------------------------------------
aux <- tibble(
  'Item' = c('II-3-15.1', 'II-3-15.2', 'II-3-15.3', 'II-3-15.4'),
  'Descrição' = c(
    'Parecer - com ART ou RRT',
    'Parecer - sem ART ou RRT',
    'Projeto ou relatório técnico - com ART ou RRT',
    'Projeto ou relatório técnico - com ART ou RRT'
  ),
  'Pontos' = as.numeric(df[[3]][c(6,9,12,15)])
)

da <- rbind(da, aux)

df <- df[-c(1:15),]

# Itens II-3-16 a II-3-18 -------------------------------------------
aux <- tibble(
  'Item' = c('II-3-16', 'II-3-17', 'II-3-18'),
  'Descrição' = c(
    'Anais, manuais, catálogos, boletins, etc. (organizador/redator)',
    'Produção e publicação de mapas, cartas ou similares',
    'Desenvolvimento de maquete'
  ),
  'Pontos' = as.numeric(df[[3]][c(3,5,7)])
)

da <- rbind(da, aux)

df <- df[-c(1:7),]

# Itens II-3-19.1 a II-3-19.2 -------------------------------------------
aux <- tibble(
  'Item' = c('II-3-19.1', 'II-3-19.2'),
  'Descrição' = c(
    'Restauração de obra artística',
    'Conservação de obra artística'
  ),
  'Pontos' = as.numeric(df[[3]][c(8,10)])
)

da <- rbind(da, aux)

df <- df[-c(1:10),]

# Itens II-3-21.1 a II-3-21.2 -------------------------------------------
aux <- tibble(
  'Item' = c('II-3-21.1', 'II-3-21.2'),
  'Descrição' = c(
    'Produção de cinema, vídeo, etc. - Editor, roteirista, diretor e produto',
    'Produção de cinema, vídeo, etc. - Participante.'
  ),
  'Pontos' = as.numeric(df[[3]][c(7,9)])
)

da <- rbind(da, aux)

df <- df[-c(1:9),]

# Itens II-3-22 e II-3-23.1 a II-3-23.3 ----------------------------------------
aux <- tibble(
  'Item' = c('II-3-22', 'II-3-23.1', 'II-3-23.2', 'II-3-23.3'),
  'Descrição' = c(
    'Criação e manutenção de páginas em Rede sociais, websites e blogs',
    'Participação em entrevista, mesa redonda - Regional/Local',
    'Participação em entrevista, mesa redonda - Nacional',
    'Participação em entrevista, mesa redonda - Internacional'
  ),
  'Pontos' = as.numeric(df[[3]][c(2,10,12,14)])
)

da <- rbind(da, aux)

# II-4 Outro Tipo de Produção --------------------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^II - 4 Outro Tipo de Produção") %>% which
end <- df %>% str_detect("^ III ± ATIVIDADES DE PESQUISA E DE EXTENSÃO") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens II-4-1 a II-4-8 -------------------------------------------
aux <- tibble(
  'Item' = c('II-4-1', 'II-4-2', 'II-4-3', 'II-4-4',
            'II-4-5', 'II-4-6', 'II-4-7', 'II-4-8'),
  'Descrição' = c(
    'Artigos de opinião veiculados em jornais e revistas',
    'Texto ou material didático para uso institucional',
    'Artigos de divulgação científica, tecnológica e artística, etc.',
    'Apresentação oral de trabalho em congresso científico',
    'Apresentação de pôsteres em congresso científico',
    'Organização de caderno de programação e resumos de eventos',
    'Trabalho premiado em evento científico nacional ou internacional',
    'Tese, dissertação ou trabalho de iniciação científica premiados'
  ),
  'Pontos' = c(1,2,3,3,1,3,5,8)
)

da <- rbind(da, aux)

# III - 1 Atividades de Coordenação de Pesquisa e Inovação  ------------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^ III ± ATIVIDADES DE PESQUISA E DE EXTENSÃO") %>% which
end <- df %>% str_detect("^ III - 2 Atividades de Extensão ") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens III-1-1 a III-1-3 -------------------------------------------
aux <- tibble(
  'Item' = c('III-1-1', 'III-1-2', 'III-1-3'),
  'Descrição' = c(
    'Coordenador de projeto conjuntos de pesquisa e cooperação científica',
    'Coordenador de projeto de pesquisa ou inovação com financiamento',
    'Coordenador de projeto de pesquisa ou inovação sem financiamento'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,8,11)])
)

da <- rbind(da, aux)

# III-2 Atividades de Extensão  ------------------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^ III - 2 Atividades de Extensão") %>% which
end <- df %>% str_detect("^     IV - ATIVIDADES ADMINISTRATIVAS") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens III-2-1 a III-2-7 -------------------------------------------
aux <- tibble(
  'Item' = c('III-2-1', 'III-2-2', 'III-2-3', 'III-2-4',
            'III-2-5', 'III-2-6', 'III-2-7'),
  'Descrição' = c(
    'Coordenador de programa ou projeto de extensão com financiamento',
    'Coordenador de programa ou projeto de extensão/cultura cadastrado na PROEC',
    'Coordenador de contratos ou convênios de cooperação internacional',
    'Coordenador de contratos ou convênios de cooperação nacional',
    'Participante de projeto de extensão/cultura cadastrado na PROEC',
    'Curso de extensão ministrado com 20 ou mais horas',
    'Curso de extensão ministrado com menos de 20 horas'
  ),
  'Pontos' = as.numeric(df[[3]][c(3,6,9,11,13,16,19)])
)

da <- rbind(da, aux)
df <- df[-c(1:19),]

# Itens III-2-8.1 a III-2-8.3 -------------------------------------------
aux <- tibble(
  'Item' = c('III-2-8.1', 'III-2-8.2', 'III-2-8.3'),
  'Descrição' = c(
    'Palestrante, participante de mesa redonda em evento - Internacional',
    'Palestrante, participante de mesa redonda em evento - Nacional',
    'Palestrante, participante de mesa redonda em evento - Regional ou local'
  ),
  'Pontos' = as.numeric(df[[3]][c(6,8,10)])
)

da <- rbind(da, aux)
df <- df[-c(1:10),]

# Itens III-2-9.1 a III-2-9.1 -------------------------------------------
aux <- tibble(
  'Item' = c('III-2-9.1', 'III-2-9.2'),
  'Descrição' = c(
    'Promoção ou produção de eventos locais - Presidente',
    'Promoção ou produção de eventos locais - Comissão organizadora'
    ),
  'Pontos' = as.numeric(df[[3]][c(4,6)])
)

da <- rbind(da, aux)
df <- df[-c(1:6),]

# Itens III-2-10.1 a III-2-10.1 -------------------------------------------
aux <- tibble(
  'Item' = c('III-2-10.1', 'III-2-10.2'),
  'Descrição' = c(
    'Promoção ou produção de eventos regionais - Presidente',
    'Promoção ou produção de eventos regionais - Comissão organizadora'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6)])
)

da <- rbind(da, aux)
df <- df[-c(1:6),]

# Itens III-2-11.1 a III-2-11.1 -------------------------------------------
aux <- tibble(
  'Item' = c('III-2-11.1', 'III-2-11.2'),
  'Descrição' = c(
    'Promoção ou produção de eventos nacionais - Presidente',
    'Promoção ou produção de eventos nacionais - Comissão organizadora'
  ),
  'Pontos' = as.numeric(df[[3]][c(8,10)])
)

da <- rbind(da, aux)
df <- df[-c(1:10),]

# Itens III-2-12.1 a III-2-12.1 -------------------------------------------
aux <- tibble(
  'Item' = c('III-2-12.1', 'III-2-12.2'),
  'Descrição' = c(
    'Promoção ou produção de eventos internacionais - Presidente',
    'Promoção ou produção de eventos internacionais - Comissão organizadora'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6)])
)

da <- rbind(da, aux)

# IV-1 Direção e Função Gratificada --------------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^     IV - ATIVIDADES ADMINISTRATIVAS") %>% which
end <- df %>% str_detect("^IV ± 2 Atividades Administrativas") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens IV-1-1 a IV-2-15 -------------------------------------------
aux <- tibble(
  'Item' = c('IV-1-1', 'IV-1-2', 'IV-1-3', 'IV-1-4',
            'IV-1-5', 'IV-1-6', 'IV-1-7','IV-1-8',
            'IV-1-9', 'IV-1-10', 'IV-1-11', 'IV-1-12',
            'IV-1-13', 'IV-1-14', 'IV-1-15'),
  'Descrição' = c(
    'Reitor ou Vice-Reitor ou Pró-Reitor',
    'Diretor de Regional da UFG',
    'Vice-Diretor de Regional da UFG',
    'Coordenadores das Regionais paralelos aos Pró-Reitores da UFG',
    'Chefe de Gabinete da Reitoria',
    'Coordenador ou Assessor vinculado à Reitoria',
    'Assessor vinculado à Diretoria de Regional',
    'Diretor de Unidade Acadêmica',
    'Diretor Geral do Hospital das Clínicas',
    'Coordenador ou Assessor vinculado às Pró-Reitorias',
    'Coordenador de Programa de Pós-Graduação stricto sensu',
    'Coordenador de Curso de Ensino Básico ou de Graduação',
    'Vice-Diretor de Unidade Acadêmica',
    'Diretor do Hospital Veterinário',
    'Diretor de Órgão da Administração'
  ),
  'Pontos' = as.numeric(df[[3]][c(5,7,9,11,13,15,17,19,21,23,26,28,30,33,35)])
)

da <- rbind(da, aux)
df <- df[-c(1:35),]

# IV-2 Atividades Administrativas --------------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^IV ± 2 Atividades Administrativas") %>% which
end <- df %>% str_detect("^IV ± 3 Outras Atividades Administrativas ") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens IV-2-1 a IV-2-5.1 -------------------------------------------
aux <- tibble(
  'Item' = c('IV-2-1', 'IV-2-2', 'IV-2-3', 'IV-2-4','IV-2-5', 'IV-2-5.1'),
  'Descrição' = c(
    'Coordenador de projeto institucional com financiamento',
    'Coordenador de curso de especialização',
    'Vice-Diretor do CIAR, de Cursos de Graduação ou de Pós-Graduação',
    'Membro representante de classe da carreira docente no CONSUNI',
    'Membro do Conselho de Curadores ou das Câmaras Superiores Setoriais',
    'Membro do Conselho Gestor das Regionais ou das Câmaras Regionais Setoriais'
  ),
  'Pontos' = as.numeric(df[[3]][c(3,6,9,12,14,17)])
)

da <- rbind(da, aux)
df <- df[-c(1:17),]

# Itens IV-2-6.1 a IV-2-6.6 -------------------------------------------
aux <- tibble(
  'Item' = c('IV-2-6.1', 'IV-2-6.2', 'IV-2-6.3', 'IV-2-6.4','IV-2-6.5', 'IV-2-6.6'),
  'Descrição' = c(
    'Atividades designadas por portaria - Com carga horária menor ou igual a 30 horas',
    'Atividades designadas por portaria - Com carga horária maior do que 30 horas e menor ou igual a 60 horas',
    'Atividades designadas por portaria - Com carga horária maior do que 60 horas e menor ou igual a 90 horas',
    'Atividades designadas por portaria - Com carga horária maior do que 90 horas e menor ou igual a 120 horas',
    'Atividades designadas por portaria - Com carga horária maior do que 120 horas e menor ou igual a 150 horas',
    'Atividades designadas por portaria - Com carga horária maior do que 150 horas'
    ),
  'Pontos' = as.numeric(df[[3]][c(6,8,10,12,14,16)])
)

da <- rbind(da, aux)

# IV ± 3 Outras Atividades Administrativas -------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^IV ± 3 Outras Atividades Administrativas") %>% which
end <- df %>% str_detect("^IV ± 4 Atividades de Representação Fora da UFG") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens IV-3-1 a IV-3-10 -------------------------------------------
aux <- tibble(
  'Item' = c('IV-3-1', 'IV-3-2', 'IV-3-3', 'IV-3-4','IV-3-5',
            'IV-3-6', 'IV-3-7', 'IV-3-8', 'IV-3-9','IV-3-10'),
  'Descrição' = c(
    'Presidente da CPPD',
    'Presidente dos Comitês de Ética em Pesquisa (CEP ou CEUA)',
    'Presidente da Comissão de Avaliação Institucional ou da Comissão Própria de Avaliação',
    'Membros da Coordenação Permanente do Centro de Seleção',
    'Diretores do Hospital das Clínicas',
    'Membros da CPPD, da Comissão de Avaliação Institucional, da Comissão Própria de Avaliação, da CAD',
    'Membros da CPAD ou da Comissão de Sindicância ou da Comissão de Processo Administrativo',
    'Membro do NDE',
    'Gestor de Convênios/Projetos Internacionais da Coordenadoria de Assuntos Internacionais',
    'Coordenador da Comissão de Pesquisa/Ensino/Extensão/Estágio das UAs'
     ),
  'Pontos' = as.numeric(df[[3]][c(3,5,8,10,12,14,21,24,26,28)])
)

da <- rbind(da, aux)
df <- df[-c(1:28),]

# Itens IV-3-11 a IV-3-19 -------------------------------------------
aux <- tibble(
  'Item' = c('IV-3-11', 'IV-3-12', 'IV-3-13', 'IV-3-14','IV-3-15',
            'IV-3-16', 'IV-3-17', 'IV-3-18', 'IV-3-19'),
  'Descrição' = c(
    'Chefia de Departamento e respectivo vice ou atividade equivalente',
    'Chefe do Pronto Socorro ou da Maternidade ou do CEROF',
    'Membros dos Comitês de Ética em Pesquisa (CEP ou CEUA)',
    'Membros do Comitê Interno do PIBIC e do PIBITI',
    'Orientador Técnico Titular de Empresa Júnior',
    'Orientador Técnico Colaborador de Empresa Júnior',
    'Coordenador de Monitoria',
    'Coordenador de Módulo de Metodologia Ativa',
    'Coordenador de TCC ou de Prática como Componente Curricular'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6,9,12,14,16,18,20,22)])
)

da <- rbind(da, aux)
df <- df[-c(1:22),]

# Itens IV-3-20.1 a IV-3-20.4 -------------------------------------------
aux <- tibble(
  'Item' = c('IV-3-20.1', 'IV-3-20.2', 'IV-3-20.3', 'IV-3-20.4'),
  'Descrição' = c(
    'Editor de revistas, periódicos ou jornais - Com classificação Qualis A',
    'Editor de revistas, periódicos ou jornais - Com classificação Qualis B',
    'Editor de revistas, periódicos ou jornais - Com classificação Qualis C',
    'Editor de revistas, periódicos ou jornais - Sem classificação Qualis'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6,8,10)])
)

da <- rbind(da, aux)
df <- df[-c(1:10),]

# Itens IV-3-21 a IV-3-22 -------------------------------------------
aux <- tibble(
  'Item' = c('IV-3-21', 'IV-3-22'),
  'Descrição' = c(
    'Membro de comitê de assessoramento de agencias oficiais de fomento',
    'Membros de Comissões/Conselhos/Comitês de Órgãos Governamentais'
  ),
  'Pontos' = as.numeric(df[[3]][c(2,5)])
)

da <- rbind(da, aux)

# IV ± 3 Outras Atividades Administrativas -------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

ini <- df %>% str_detect("^IV ± 4 Atividades de Representação Fora da UFG") %>% which
end <- df %>% str_detect("^ V - OUTRAS ATIVIDADES") %>% which

df <- df[ini:end]

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens IV-4-1 a IV-4-6 -------------------------------------------
aux <- tibble(
  'Item' = c('IV-4-1', 'IV-4-2', 'IV-4-3', 'IV-4-4','IV-4-5', 'IV-4-6'),
  'Descrição' = c(
    'Representante titular em conselho de classe profissional com carga horária igual ou superior a 150 horas',
    'Presidente do Sindicato de Docentes da UFG',
    'Diretor do Sindicato de Docentes da UFG',
    'Representante sindical com carga horária igual ou superior a 150 horas',
    'Representante em entidade científica, artística e cultural com carga horária igual ou superior a 150 horas',
    'Representante em comissão de órgão governamental com carga horária igual ou superior a 150 horas'
  ),
  'Pontos' = as.numeric(df[[3]][c(3,6,8,10,12,15)])
)

da <- rbind(da, aux)

# V - 1 Atividades Acadêmicas ± Orientação  ------------------------------------
tabela <- tabela[end:length(tabela)]
df <- tabela

df <- df %>%
  str_replace_all(., ":", "\\|") %>%
  str_replace_all(.,  "\\s{2,}", "\\|")

df <-df %>%
  as.tibble() %>%
  separate_wider_delim(value,  '|',
                       names = letters[1:5],
                       cols_remove = TRUE,
                       too_few = "align_start",
                       too_many = "merge")

df[[3]] <- str_extract_all(df[[3]], '.*[^*]')

# Itens V-1-1 a IV-1-12 -------------------------------------------
aux <- tibble(
  'Item' = c('V-1-1', 'V-1-2', 'V-1-3', 'V-1-4','V-1-5', 'V-1-6',
            'V-1-7', 'V-1-8', 'V-1-9', 'V-1-10','V-1-11', 'V-1-12'),
  'Descrição' = c(
    'Aluno orientado em tese de doutorado defendida e aprovada',
    'Aluno co-orientado em tese de doutorado defendida e aprovada',
    'Aluno orientado em tese de doutorado em andamento',
    'Aluno co-orientado em tese de doutorado em andamento',
    'Aluno orientado em dissertação de mestrado defendida e aprovada',
    'Aluno co-orientado em dissertação de mestrado defendida e aprovada',
    'Aluno orientado em dissertação de mestrado em andamento',
    'Aluno co-orientado em dissertação de mestrado em andamento',
    'Aluno orientado em monografia de especialização aprovada',
    'Aluno orientado em monografia de especialização em andamento',
    'Aluno orientado em residência médica',
    'Aluno orientado em estágio curricular obrigatório'
  ),
  'Pontos' = as.numeric(df[[3]][c(4,6,8,10,12,14,16,18,20,22,25,27)])
)

da <- rbind(da, aux)
df <- df[-c(1:27),]

# Itens V-1-13 a IV-1-24 -------------------------------------------
aux <- tibble(
  'Item' = c('V-1-13', 'V-1-14', 'V-1-15', 'V-1-16','V-1-17', 'V-1-18',
            'V-1-19', 'V-1-20', 'V-1-21', 'V-1-22','V-1-23', 'V-1-24'),
  'Descrição' = c(
    'Aluno orientado em projeto de final de curso',
    'Aluno de outra IFE orientado em tese de doutorado defendida e aprovada',
    'Aluno de outra IFE co-orientado em tese de doutorado defendida e aprovada',
    'Aluno de outra IFE orientado em tese de doutorado em andamento',
    'Aluno de outra IFE co-orientado em tese de doutorado em andamento',
    'Aluno de outra IFE orientado em dissertação de mestrado defendida e aprovada',
    'Aluno de outra IFE co-orientado em dissertação de mestrado defendida e aprovada',
    'Aluno de outra IFE orientado em dissertação de mestrado em andamento',
    'Aluno de outra IFE co-orientado em dissertação de mestrado em andamento',
    'Aluno orientado em programas institucionais de iniciação científica',
    'Aluno orientado em programas institucionais de iniciação científica júnior',
    'Aluno orientado em programa especial de treinamento (PET)'
  ),
  'Pontos' = as.numeric(df[[3]][c(2,4,6,12,14,16,18,20,22,24,28,31)])
)

da <- rbind(da, aux)
df <- df[-c(1:31),]

# Itens V-1-25 a IV-1-35 -------------------------------------------
aux <- tibble(
  'Item' = c('V-1-25', 'V-1-26', 'V-1-27', 'V-1-28','V-1-29', 'V-1-30',
            'V-1-31', 'V-1-32', 'V-1-33', 'V-1-34','V-1-35'),
  'Descrição' = c(
    'Aluno com bolsa orientado em projetos de pesquisa/inovação/extensão/cultura/ensino',
    'Aluno sem bolsa orientado em projetos de pesquisa/inovação/extensão/cultura/ensino',
    'Aluno orientado em programa de monitoria',
    'Aluno orientado em estágio curricular não obrigatório ou estágio docência',
    'Aluno orientado em prática como componente curricular (PCC)',
    'Aluno com deficiência, transtornos globais do desenvolvimento e altas habilidades/superdotação orientado em programa de apoio pedagógico ou em trabalho final de curso',
    'Pesquisador supervisionado em estágio de pós-doutoramento (PRODOC, PNPD, DCR, PDJ, PDS e similares)',
    'Aluno orientado em atividade de Preceptoria',
    'Aluno orientado em atividade de Tutoria',
    'Aluno orientado em Programa de Intercâmbio Internacional',
    'Aluno de baixo rendimento acompanhado/orientado por meio de um projeto de ensino'
  ),
  'Pontos' = as.numeric(df[[3]][c(2,4,6,8,10,12,16,19,21,23,25)])
)

da <- rbind(da, aux)
df <- df[-c(1:25),]

# Itens V-2-1.1 a V-2-3.2 -------------------------------------------
aux <- tibble(
  'Item' = c('V-2-1.1', 'V-2-1.2', 'V-2-2', 'V-2-3.1', 'V-2-3.2'),
  'Descrição' = c(
    'Membro de banca de concurso para docente efetivo - Na instituição',
    'Membro de banca de concurso para docente efetivo - Em outra instituição',
    'Membro de banca de concurso para docente substituto',
    'Membro de banca de defesa de dissertação de mestrado - Na instituição',
    'Membro de banca de defesa de dissertação de mestrado - Em outra instituição'
  ),
  'Pontos' = as.numeric(df[[3]][c(11,13,15,19,21)])
)

da <- rbind(da, aux)
df <- df[-c(1:21),]

# Itens V-2-4.1 a V-2-6.2 -------------------------------------------
aux <- tibble(
  'Item' = c('V-2-4.1', 'V-2-4.2', 'V-2-5.1', 'V-2-5.2', 'V-2-6.1', 'V-2-6.2'),
  'Descrição' = c(
    'Membro de banca de defesa de tese de doutorado - Na instituição',
    'Membro de banca de defesa de tese de doutorado - Em outra instituição',
    'Membro de banca de qualificação de mestrado - Na instituição',
    'Membro de banca de qualificação de mestrado - Em outra instituição',
    'Membro de banca de qualificação de doutorado - Na instituição',
    'Membro de banca de qualificação de doutorado - Em outra instituição'
  ),
  'Pontos' = as.numeric(df[[3]][c(8,10,14,16,20,22)])
)

da <- rbind(da, aux)
df <- df[-c(1:22),]

# Itens V-2-7.1 a V-2-11 -------------------------------------------
aux <- tibble(
  'Item' = c('V-2-7.1', 'V-2-7.2', 'V-2-8', 'V-2-9.1', 'V-2-9.2',
            'V-2-10', 'V-2-11'),
  'Descrição' = c(
    'Membro de CEA ou de tese inédita para promoção à Classe E - Na instituição',
    'Membro de CEA ou de tese inédita para promoção à Classe E - Em outra instituição',
    'Membro de banca de defesa de monografia, projeto final de curso, etc',
    'Membro de corpo de júri - Concursos internacionais',
    'Membro de corpo de júri - Concursos nacionais',
    'Cursos/palestras ministrados para docentes/funcionários/alunos da UFG',
    'Coordenador de projeto institucional de intercâmbio internacional'
  ),
  'Pontos' = as.numeric(df[[3]][c(5,7,9,14,16,18,21)])
)

da <- rbind(da, aux)
df <- df[-c(1:21),]

# Itens V-3-1 a V-3-6 -------------------------------------------
aux <- tibble(
  'Item' = c('V-3-1', 'V-3-2', 'V-3-3', 'V-3-4', 'V-3-5', 'V-3-6'),
  'Descrição' = c(
    'Docente regularmente matriculado em curso de Pós-Graduação stricto sensu',
    'Estágio Pós-Doutoral ou Estágio Sênior',
    'Docente em licença para capacitação',
    'Curso de aperfeiçoamento com carga horária igual ou superior a 40 horas',
    'Curso de aperfeiçoamento com carga horária inferior a 40 horas',
    'Participação em Congressos, Seminários, Encontros, Jornadas etc. '
  ),
  'Pontos' = as.numeric(df[[3]][c(9,12,14,18,20,22)])
)

da <- rbind(da, aux)

anexoII <- da

save(anexoII, file = 'data/anexoII.rda')
