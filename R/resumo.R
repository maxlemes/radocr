#
# ---------------------  Função EXTERNA - resumo_tex  ----------------------
#
#' Cria um resumo (em LaTeX) das atividades docentes no período
#' 
#' @description
#' Esta função lê os dados do RADOC em 2 arquivos PDF e retorna um
#' arquivo LaTeX (.tex) com o resumo das atividades do docente
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
#' resumo_tex(pdf_radoc1, pdf_radoc2, output_file)
#' }
#' @export
resumo_tex <- function(pdf_file1, pdf_file2, output_file=NULL) {

  # Check PDF files
  filesCheck(pdf_file1, pdf_file2)

  # pegando os dados do docente ------------------------------------------------
  doc <- docente(pdf_file1, pdf_file2)[[2]][1]

  # gerando o resumo dos dados em Latex ----------------------------------------
  
  files <- c(pdf_file1, pdf_file2)
  titulo <- NULL
  resumo <- NULL

  for (file in files) {

    df <- pontua(file)

    # criando o título e alterando a coluna de Pontos
    tit <- paste('Resumo das atividades do ano', colnames(df)[4])
    titulo <- append(titulo, tit)

    colnames(df)[4] <- 'Pontos'

    # transforma o dataframe em tabela
    df <- xtable::xtable(df)

    # alinhamento das colunas
    xtable::align(df) <- "l|l|p{12cm}|r|r|"

    # # criando o arquivo com a tabela
    bold <- function(x) {
      paste("{\\textbf{", x, "}}", sep = "")
    }
    gray <- function(x) {
      paste("{\\textcolor{gray}{", x, "}}", sep = "")
    }

    #  gerando o arquivo com a tabela para LaTeX
    res <- print(df,
      # scalebox = 0.9,
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
    resumo <- append(resumo, res)
  }

  # gerando as linhas de comando do arquivo Latex ------------------------------
  latex_content <- c(
    "\\documentclass[11pt,a4paper]{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{geometry}",
    "\\usepackage[table]{xcolor} %for use in color links",
    "\\usepackage{graphicx}",
    "",
    "% definindo as margens do documento",
    "\\geometry{a4paper,text={17.5cm,27.5cm},centering}",
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
    "\\noindent\\textbf{Docente:}", doc, 
    "",
    "\\begin{center}",
    "\\large \\textbf{", titulo[[1]], "}",
    "\\end{center}",
    "\\vspace{-0.5cm}",
    resumo[[1]],
    "",
    "\\newpage",
    "",
    "\\begin{center}",
    "  \\textbf{UNIVERSIDADE FEDERAL DE GOI\u00c1S\\\\",
    "  INSTITUTO DE MATEM\u00c1TICA E ESTAT\u00cdSTICA}",
    "\\end{center}",
    "",
    "\\noindent\\textbf{Docente:}", doc, 
    "",
    "\\begin{center}",
    "\\large \\textbf{", titulo[[2]], "}",
    "\\end{center}",
    "\\vspace{-0.5cm}",
    resumo[[2]],
    "",
    "\\end{document}"
  )

  # definindo o arquivo de saida padrão
  if (is.null(output_file)) {
    output_file <- file.path(getwd(), "resumo.tex")
  }

  # Escrevendo o arquivo LaTeX
  sink(tempfile()) # Redireciona a saída padrão para um arquivo temporário
  writeLines(latex_content, output_file)
  sink() # Restaura a saída padrão para o console

  # return(paste("O arquivo", output_file, "foi criado"))
  return("Resumo criado com sucesso.")

}

#
# --------------------  Função EXTERNA - resumo_xlsx  ----------------------
#
#' Cria um resumo (em Excel) das atividades docentes no período
#' 
#' @description
#' Esta função lê os dados do RADOC em 2 arquivos PDF cria uma planilha (em
#' Excel) a partir dos RADOCs com o resumo das atividades do docente avaliado
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
#'  \dontrun{
#' pdf_radoc1 <- "pasta_do_arquivo/radoc1.pdf"
#' pdf_radoc2 <- "pasta_do_arquivo/radoc2.pdf"
#' output_file <- "pasta_de_destino/nome_arquivo.xlsx"
#' resumo_xlsx(pdf_radoc1, pdf_radoc2, output_file)
#' }
#' @export
resumo_xlsx <- function(pdf_file1, pdf_file2, output_file=NULL) {

  # Check PDF files
  filesCheck(pdf_file1, pdf_file2)

  # colocando os arquivos num vetor
  files <- c(pdf_file1, pdf_file2)

  # atribuições iniciais
  titulo    <- NULL
  resumo    <- NULL
  cabecalho <- NULL

  ## Criando uma planilha vazia
  wb_res <- openxlsx2::wb_workbook()
  
  for (file in files) {

    # pegando os dados do docente
    doc <- docente(pdf_file1, pdf_file2)[1,]
  
    # capturando os dados
    df <- pontua(file)

    # criando o título e alterando a coluna de Pontos
    ano <- colnames(df)[[4]]
    titulo  <- paste('Resumo das atividades do ano', ano)

    # capturando o cabecalho
    colnames(df)[4] <- 'Pontos'
    cabecalho <- colnames(df)

    # criando linhas dicionais
    doc[(nrow(doc) + 1), ] <- as.list(c("", ""))
    doc[(nrow(doc) + 1), ] <- as.list(c("", ""))
    doc[(nrow(doc) + 1), ] <- as.list(c("", ""))
    doc[(nrow(doc) + 1), ] <- as.list(c("", ""))

    # criando colunas adicionais
    doc[, "c"] <- as.integer("")
    doc[, "d"] <- as.numeric("")

    # igualando os nomes
    colnames(doc) <- colnames(df)

    # combinando os dataframes
    df <- rbind(doc, df)

    # extraindo o inicio da tabela de dados
    n_res <- which(stringr::str_detect(df[[1]], paste0("^I")))[[1]]

    # adicionando uma aba
    wb_res$add_worksheet(sheet = ano)

    # adicionando dos dados na planilha
    wb_res$add_data(
      sheet = ano,
      df,
      na.strings = '',
      row_names = FALSE,
      col_names = FALSE
    )

    # adicionando o titulo na planilha
    wb_res$add_data(
      sheet = ano,
      titulo,
      dims = 'B3',
      na.strings = '',
      row_names = FALSE,
      col_names = FALSE
    )

      # adicionando o cabeçalho na planilha
      wb_res$add_data(
        sheet = ano,
        cabecalho,
        dims = 'A5:D5',
        na.strings = '',
        row_names = FALSE,
        col_names = FALSE
      )

    # editando a fonte da planilha
    wb_res$add_font(
      sheet =  ano,
      dims = "A1:Z80",
      name = "Calibri",
      size = "16"
    )

    # colocando a primeira coluna docente em negrito
    wb_res$add_font(
      sheet =  ano,
      dims = paste0("A", 1, ":A", nrow(doc)),
      name = "Calibri",
      size = "16",
      bold = TRUE
    )

    # colocando as linhas destacas em negrito
    for (i in c(3,5)) {
      wb_res$add_font(
        sheet = ano,
        dims = paste0("A", i, ":D", i),
        name = "Calibri",
        size = "16",
        bold = TRUE
      )
    }

    # colorindo as linhas alternadamentes no resto da tabela
    for (i in seq(from = n_res, to = nrow(df), by = 2)) {
      cells <- paste0("A", i, ":D", i)
      wb_res$add_fill(
        sheet =  ano,
        dims = cells,
        color = openxlsx2::wb_color(hex = "#d3d3d3")
      )
    }

    # ajustando a largura das colunas
    wb_res$set_col_widths(
      sheet =  ano,
      cols = 1:4,
      widths = "auto"
    )
  }

  if (is.null(output_file)) {
    output_file <- file.path(getwd(), "resumo.xlsx")
  }

  # salvando a tabela no discos
  openxlsx2::wb_save(wb_res,
    file = output_file, 
    overwrite = TRUE)

  # return(paste("O arquivo", output_file, "foi criado"))
  return("Resumo criado com sucesso.")
}

