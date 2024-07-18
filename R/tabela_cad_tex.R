#' Create a LaTeX Document from PDF Data
#'
#' This function reads data from tow PDF files and creates a LaTeX (.tex) 
#' document containing a table with the datas.
#'
#' @param pdf_file1 Path to the PDF file 1.
#' @param pdf_file2 Path to the PDF file 2.
#' @param n,  carga horária do docende (padrão n=40)
#' 
#' @param output_file Path to the output Latex (.tex) file.
#' @export
#' 
#' @examples
#'\dontrun{
#' tabela_cad_tex(pdf_file1, pdf_file2, output_file)
#' }

tabela_cad_tex <- function(pdf_file1, pdf_file2, output_file, n=40) {
  
  # Check PDF files -------------------------------------------------------------------------
  radocr:::filesCheck(pdf_file1, pdf_file2)

  # pegando os dados do docente
  doc <- radocr:::final_doc(pdf_file1, pdf_file2)

  # transforma em uma tabela de latex
  df <- xtable::xtable(doc)

  # alinhamento das colunas
  xtable::align(df) <- c(rep('l',2),'l')

    # colocando linhas em negrito
    for (j in 1:nrow(df)){
      df[j,1] = paste0('\\textbf{',df[j,1],'}')
    }

  # gerando a tabela em LaTeX
  table_doc <- print(df,
                  scalebox = 1.1,
                  hline.after = c(-1,nrow(df)),
                  sanitize.text.function=function(x){x},
                  include.rownames=FALSE,
                  include.colnames=FALSE,
                  type = "latex")
  
  
  # gerando a tabela da CAD em Latex
  cad <- radocr:::final_cad(pdf_file1, pdf_file2)

  df <- xtable::xtable(cad)

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

  #  gerando o arquivo com a tabela para LaTeX
  table_cad <- print(df,
    
                  scalebox = 0.9,
                  hline.after = c(-1,0, nrow(df)),
                  include.rownames=FALSE,
                  include.colnames=TRUE,
                  sanitize.text.function=function(x){x},
                  sanitize.rownames.function=gray,
                  sanitize.colnames.function=bold,
                  type = "latex")
  
  
  # gerando as linhas de comando do arquivo Latex
  latex_content <- c(
    "\\documentclass[11pt,a4paper]{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{geometry}",
    "\\usepackage[table]{xcolor} %for use in color links",
    "\\usepackage{graphicx}",
    "",
    "% definindo as margens do documento",
    "\\geometry{a4paper,text={17.5cm,27cm},centering}",
    "",
    "% Definindo as cores das linhas da Tabela",
    "\\definecolor{lightgray}{gray}{0.9}",
    "\\rowcolors{1}{white}{lightgray}",
    "",
    "\\begin{document}",
    "\\thispagestyle{empty}",
    "",
    "\\begin{center}",
    "  \\textbf{UNIVERSIDADE FEDERAL DE GOIÁS\\\\",
    "  INSTITUTO DE MATEMÁTICA E ESTATÍSTICA}",
    "\\end{center}",
    "",
    "\\vspace{1cm}",
    "",
    "\\begin{center}",
    "\\large \\textbf{Dados do Docente}",
    "\\end{center}",
    "\\centering",
    table_doc,
    "",
    "\\begin{center}",
    "\\large \\textbf{Tabela de Avaliação da CAD}",
    "\\end{center}",
    "\\centering",
    table_cad,
    "",
    "\\end{document}"
  )

  # Write the LaTeX content to the output file
  writeLines(latex_content, output_file)
}