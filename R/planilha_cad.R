planilha_cad <- function (docente, tab_cad) {

  doc <- docente
  aux <- tab_cad
  
  doc[(nrow(doc)+1),] <- as.list(c('', ''))
  doc[(nrow(doc)+1),] <- as.list(c('', ''))
  doc[, 'c'] <- ''
  doc[, 'd'] <- ''
  doc[, 'e'] <- ''

  aux <- aux[c(1,1:nrow(aux)),]
  aux[1,] <- as.list(colnames(aux))
  colnames(aux) <- colnames(doc)

  aux <- rbind(doc, aux)
  n_aux <- which(stringr::str_detect(aux[[ 1 ]], paste0('^Item')))

  ## Criando uma planilha
  wb_cad <- openxlsx2::wb_workbook()

  # adicionando uma aba
  wb_cad$add_worksheet(sheet = "NotasCad")

  # adicionando dos dados na planilha
  wb_cad$add_data(sheet = 'NotasCad',
                  aux,
                  row_names = FALSE,
                  col_names = FALSE)
  
  # editando a fonte da planilha
  wb_cad$add_font(sheet = "NotasCad",
                  dims = 'A1:Z80',
                  name = "Calibri",
                  size = "16")
  
  # colocando a primeira coluna docente em negrito
  wb_cad$add_font(sheet = "NotasCad",
                  dims = paste0('A',1, ":A", nrow(doc)),
                  name = "Calibri",
                  size = "16",
                  bold = TRUE)
  
  # colocando as linhas destacas em negrito
  for(item in c('^Item', 'I', 'II', "III", "IV", 'V', 'P', 'NF', 'S')) {
    i <- which(grepl(paste0('^',item, '$'), aux[[1]]))
    wb_cad$add_font(sheet = "NotasCad",
                    dims = paste0('A',i, ":E", i),
                    name = "Calibri",
                    size = "16",
                    bold = TRUE)
  }

  # colorindo as linhas alternadamentes nos dados do docente
  for (i in seq(from = 1, to = nrow(doc)-2, by = 2)) {
    cells <- paste0('A',i, ":B", i)
    wb_cad$add_fill(sheet = "NotasCad",
                    dims = cells,
                    color = openxlsx2::wb_color(hex = "#d3d3d3")
    )
  }

   # colorindo as linhas alternadamentes no resto da tabela
  for (i in seq(from = n_aux, to = nrow(aux), by = 2)) {
    cells <- paste0('A',i, ":E", i)
    wb_cad$add_fill(sheet = "NotasCad",
                    dims = cells,
                    color = openxlsx2::wb_color(hex = "#d3d3d3")
    )
  }

  # ajustando a largura das colunas
  wb_cad$set_col_widths(sheet = "NotasCad",
                        cols = 1:5, 
                        widths = "auto")
  
  # Ajustando os alinhamentos das colunas
  wb_cad$add_cell_style(sheet = "NotasCad",
                        i <- which(grepl('^I$', aux[[1]])),
                        dims = paste0('C',i, ":E", nrow(aux)),
                        horizontal = 'center')

  return(wb_cad)
}