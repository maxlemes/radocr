
alunos <- paste0('data-raw/',files[1])

tabela <- alunos %>%
  pdftools::pdf_ocr_text(page=1) %>%
  readr::read_lines()

# Localizando os dados do docente na tabela
ini <- tabela %>% str_detect("^Componente Curricular") %>% which %>% first
end <- tabela %>% str_detect("^\\[1\\]") %>% which %>% first

df <- tabela[(ini+2):(end-1)]

df <- df %>%
  str_replace_all(.,  "\\|", "") %>%
  str_replace_all(.,  "\\s{1,}", "\\|") %>%
  str_replace_all(.,  "\\.", "\\|") %>%
  str_replace_all(., "\\,", "\\.") %>%
  str_replace_all(., "^\\s+|\\s+$", "\\|") %>%
  str_replace_all(.,  "^\\|", "")

df <-df %>%
  as_tibble() %>%
  separate_wider_delim(value,
                       delim = '|',
                       names_sep ='|',
                       too_few = "align_start",
                       cols_remove = TRUE)
