#
# ---------------------  Função EXTERNA - tabela_cad_tex  ----------------------
#
#' Cria a Tabela Cad (em LaTeX) a partir dos RADOCs
#' 
#' @description
#' Esta função lê os dados do RADOC em 2 arquivos PDF e retorna um
#' arquivo LaTeX (.tex) com a Tabela Cad preenchida.
#'
#' @param pdf_file1 o caminho para o arquivo contendo o 1o RADOC.
#' @param pdf_file2 o caminho para o arquivo contendo o 2o RADOC.
#' @param output_file o caminho onde o arquivo de saída deve ser salvo.
#' 
#' @details
#' Em caso de ausência do argumento `output_file` o arquivo será salvo na
#' pasta atual.
#'
#' @examples
#' \dontrun{
#' pdf_radoc1 <- "pasta_do_arquivo/radoc1.pdf"
#' pdf_radoc2 <- "pasta_do_arquivo/radoc2.pdf"
#' output_file <- "pasta_de_destino/nome_arquivo.tex"
#' tabela_cad_tex(pdf_radoc1, pdf_radoc2, output_file)
#' }
#' @export
tabela_cad_tex <- function(pdf_file1, pdf_file2, output_file=NULL) {

  # Check PDF files
  filesCheck(pdf_file1, pdf_file2)

  # pegando os dados do docente ------------------------------------------------
  doc <- docente(pdf_file1, pdf_file2)

  # transforma em uma tabela de latex
  df <- xtable::xtable(doc)

  # alinhamento das colunas
  xtable::align(df) <- c(rep("|l|", 2), "l|")

  # colocando linhas em negrito
  for (j in 1:nrow(df)) {
    df[j, 1] <- paste0("\\textbf{", df[j, 1], "}")
  }

  # gerando a tabela em LaTeX
  table_doc <- print(df,
    scalebox = 1.1,
    hline.after = c(-1, nrow(df)),
    sanitize.text.function = function(x) {
      x
    },
    include.rownames = FALSE,
    include.colnames = FALSE,
    type = "latex"
  )

  # gerando a tabela da CAD em Latex -------------------------------------------
  cad <- tab_cad(pdf_file1, pdf_file2)

  df <- xtable::xtable(cad)

  # alinhamento das colunas
  xtable::align(df) <- c("|l|l|l|r|r|r|")

  # colocando linhas em negrito
  itens <- c(1, 5, 10, 13, 18, 22, 23, 24)
  for (j in itens) {
    df[j, ] <- paste0("\\textbf{", df[j, ], "}")
  }

  # criando o arquivo com a tabela
  bold <- function(x) {
    paste("{\\textbf{", x, "}}", sep = "")
  }
  gray <- function(x) {
    paste("{\\textcolor{gray}{", x, "}}", sep = "")
  }

  #  gerando o arquivo com a tabela para LaTeX
  table_cad <- print(df,
    scalebox = 0.9,
    hline.after = c(-1, 0, nrow(df)),
    include.rownames = FALSE,
    include.colnames = TRUE,
    sanitize.text.function = function(x) {
      x
    },
    sanitize.rownames.function = gray,
    sanitize.colnames.function = bold,
    type = "latex"
  )

  # gerando as linhas de comando do arquivo Latex ------------------------------
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
    "  \\textbf{UNIVERSIDADE FEDERAL DE GOI\u00c1S\\\\",
    "  INSTITUTO DE MATEM\u00c1TICA E ESTAT\u00cdSTICA}",
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
    "\\large \\textbf{Tabela de Avalia\u00e7\u00e3o da CAD}",
    "\\end{center}",
    "\\centering",
    table_cad,
    "",
    "\\end{document}"
  )

  # definindo o arquivo de saida padrão
  if (is.null(output_file)) {
    output_file <- file.path(getwd(), "tabela_cad.tex")
  }

  # Escrevendo o arquivo LaTeX
  sink(tempfile()) # Redireciona a saída padrão para um arquivo temporário
  writeLines(latex_content, output_file)
  sink() # Restaura a saída padrão para o console

  # return(paste("O arquivo", output_file, "foi criado"))
  return("Tabela CAD criada com sucesso.")

}
#
# --------------------  Função EXTERNA - tabela_cad_xlsx  ----------------------
#
#' Cria a Tabela Cad (em Excel) a partir dos RADOCs
#' 
#' @description
#' Esta função lê os dados do RADOC em 2 arquivos PDF e retorna um
#' arquivo Excel (.xlsx) com a Tabela Cad preenchida.
#'
#' @param pdf_file1 o caminho para o arquivo contendo o 1o RADOC.
#' @param pdf_file2 o caminho para o arquivo contendo o 2o RADOC.
#' @param output_file o caminho onde o arquivo de saída deve ser salvo.
#' 
#' @details
#' Em caso de ausência do argumento `output_file` o arquivo será salvo na
#' pasta atual.
#'
#' @examples
#' \dontrun{
#' pdf_radoc1 <- "pasta_do_arquivo/radoc1.pdf"
#' pdf_radoc2 <- "pasta_do_arquivo/radoc2.pdf"
#' output_file <- "pasta_de_destino/nome_arquivo.xlsx"
#' tabela_cad_xlsx(pdf_radoc1, pdf_radoc2, output_file)
#' }
#' @export
tabela_cad_xlsx <- function(...) {

  output_file <- NULL

  pdf_files <- c(...)

  # Check PDF files
  filesCheck(pdf_files)

  # pegando os dados do docente
  doc <- docente(pdf_files)

  # gerando a tabela da CAD em Latex
  cad <- tab_cad(pdf_files)

  doc[(nrow(doc) + 1), ] <- as.list(c("", ""))
  doc[(nrow(doc) + 1), ] <- as.list(c("", ""))
  for (j in letters[3:(3+length(pdf_files))]){
    doc[,j] <- ""
  }

  cad <- cad[c(1, 1:nrow(cad)), ]
  cad[1, ] <- as.list(colnames(cad))
  colnames(cad) <- colnames(doc)

  cad <- rbind(doc, cad)
  n_cad <- which(stringr::str_detect(cad[[1]], paste0("^Item")))

  ## Criando uma planilha
  wb_cad <- openxlsx2::wb_workbook()

  # adicionando uma aba
  wb_cad$add_worksheet(sheet = "NotasCad")

  # adicionando dos dados na planilha
  wb_cad$add_data(
    sheet = "NotasCad",
    cad,
    row_names = FALSE,
    col_names = FALSE
  )

  # editando a fonte da planilha
  wb_cad$add_font(
    sheet = "NotasCad",
    dims = "A1:Z80",
    name = "Calibri",
    size = "16"
  )

  # colocando a primeira coluna docente em negrito
  cells <- paste0("A", 1, ":A", nrow(doc))

  wb_cad$add_font(
    sheet = "NotasCad",
    dims = cells,
    name = "Calibri",
    size = "16",
    bold = TRUE
  )

  # colocando as linhas destacas em negrito
  for (item in c("^Item", "I", "II", "III", "IV", "V", "P", "NF", "S")) {
    i <- which(grepl(paste0("^", item, "$"), cad[[1]]))
    cells <- paste0("A", i, ":", LETTERS[ncol(cad)], i)

    wb_cad$add_font(
      sheet = "NotasCad",
      dims = cells,
      name = "Calibri",
      size = "16",
      bold = TRUE
    )
  }

  # colorindo as linhas alternadamentes nos dados do docente
  for (i in seq(from = 1, to = nrow(doc) - 2, by = 2)) {
    cells <- paste0("A", i, ":B", i)

    wb_cad$add_fill(
      sheet = "NotasCad",
      dims = cells,
      color = openxlsx2::wb_color(hex = "#d3d3d3")
    )
  }

  # colorindo as linhas alternadamentes no resto da tabela
  for (i in seq(from = n_cad, to = nrow(cad), by = 2)) {
    cells <- paste0("A", i, ":", LETTERS[ncol(cad)], i)

    wb_cad$add_fill(
      sheet = "NotasCad",
      dims = cells,
      color = openxlsx2::wb_color(hex = "#d3d3d3")
    )
  }

  # ajustando a largura das colunas
  wb_cad$set_col_widths(
    sheet = "NotasCad",
    cols = 1:5,
    widths = "auto"
  )

  # Ajustando os alinhamentos das colunas
  i <- which(grepl("^I$", cad[[1]]))
  cells <- paste0("C", i, ":", LETTERS[ncol(cad)], nrow(cad))

  wb_cad$add_cell_style(
    sheet = "NotasCad",
    dims = cells,
    horizontal = "right"
  )

  if (is.null(output_file)) {
    output_file <- file.path(getwd(), "tabela_cad.xlsx")
  }

  # salvando a tabela no discos
  openxlsx2::wb_save(wb_cad, file = output_file, overwrite = TRUE)

  # return(paste("O arquivo", output_file, "foi criado"))
  return("Tabela CAD criada com sucesso.")
}
