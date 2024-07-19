#' Create a Exccel Document from PDF Data
#'
#' This function reads data from to PDF files and creates a Exccel (.xlsx) 
#' document containing a table with the datas.
#'
#' @param pdf_file1 Path to the PDF file 1.
#' @param pdf_file2 Path to the PDF file 2.
#' @param n,  carga horária do docende (padrão n=40)
#' 
#' @param output_file Path to the output Exccel (.xlsx) file.
#' @export
#' 
#' @examples
#'\dontrun{
#' tabela_cad_xlsx(pdf_file1, pdf_file2, output_file, n=40)
#' }

tabela_cad_xlsx <- function(pdf_file1, pdf_file2, output_file, n=40) {

  # Check PDF files -------------------------------------------------------------------------
  radocr:::filesCheck(pdf_file1, pdf_file2)

  # pegando os dados do docente
  doc <- radocr:::final_doc(pdf_file1, pdf_file2)

  # gerando a tabela da CAD em Latex
  cad <- radocr:::final_cad(pdf_file1, pdf_file2)
  
  doc[(nrow(doc)+1),] <- as.list(c('', ''))
  doc[(nrow(doc)+1),] <- as.list(c('', ''))
  doc[, 'c'] <- ''
  doc[, 'd'] <- ''
  doc[, 'e'] <- ''

  cad <- cad[c(1,1:nrow(cad)),]
  cad[1,] <- as.list(colnames(cad))
  colnames(cad) <- colnames(doc)

  cad <- rbind(doc, cad)
  n_cad <- which(stringr::str_detect(cad[[ 1 ]], paste0('^Item')))

  ## Criando uma planilha
  wb_cad <- openxlsx2::wb_workbook()

  # adicionando uma aba
  wb_cad$add_worksheet(sheet = "NotasCad")

  # adicionando dos dados na planilha
  wb_cad$add_data(sheet = 'NotasCad',
                  cad,
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
    i <- which(grepl(paste0('^',item, '$'), cad[[1]]))
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
  for (i in seq(from = n_cad, to = nrow(cad), by = 2)) {
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
                        i <- which(grepl('^I$', cad[[1]])),
                        dims = paste0('C',i, ":E", nrow(cad)),
                        horizontal = 'center')

  # salvando a tabela no discos
  openxlsx2::wb_save(wb_cad, file =  output_file, overwrite = TRUE)
}