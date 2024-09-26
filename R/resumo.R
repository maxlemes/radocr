#
# ---------------------  Função EXTERNA - resumo_tex  ----------------------
#
#' Gera um resumo em LaTeX das atividades docentes a partir de arquivos RADOC
#'
#' @description
#' A função `resumo_tex()` lê os dados de um ou mais arquivos PDF de RADOC e 
#' gera um arquivo LaTeX (.tex) contendo um resumo das atividades docentes no 
#' período. É útil para criar relatórios personalizados de atividades docentes.
#'
#' @param ... Um ou mais caminhos para arquivos PDF contendo os RADOCs do 
#' docente.
#'
#' @details
#' Você pode fornecer quantos arquivos de RADOC quiser, e a função os combinará 
#' em um único documento LaTeX. Os arquivos devem ser gerados pelo sistema SICAD+.
#'
#' @return Um arquivo `resumo.tex` que contém o código LaTeX do resumo das 
#' atividades do docente.
#'
#' @examples
#' \dontrun{
#' # Caminho para os arquivos RADOC
#' pdf_radoc1 <- "pasta_do_arquivo/radoc1.pdf"
#' pdf_radoc2 <- "pasta_do_arquivo/radoc2.pdf"
#'
#' # Gera o arquivo LaTeX com o resumo
#' resumo_tex(pdf_radoc1, pdf_radoc2)
#' }
#'
#' @export
resumo_tex <- function(...) {

  output_file <- NULL

  pdf_files <- c(...)

  # Check PDF files
  filesCheck(pdf_files)

  # pegando os dados do docente ------------------------------------------------
  doc <- docente(pdf_files)[[2]][1]

  # gerando o resumo dos dados em Latex ----------------------------------------
  
  pdf_files <- c(...)
  titulo <- NULL
  resumo <- NULL

  for (file in pdf_files) {

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
    "\\begin{document}")
  
    for (i in 1:length(pdf_files)){
      aux <- c(
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
        "\\large \\textbf{", titulo[[i]], "}",
        "\\end{center}",
        "\\vspace{-0.5cm}",
        resumo[[i]],
        "",
        "\\newpage"
      )
      latex_content <- append(latex_content, aux)
    }
    latex_content <- append(
      latex_content, 
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
# --------------------  Função INTERNA - resumo_xlsx  ----------------------
#
#' Acrescenta o resumo das atividades docentes na tabela criada pela função
#' tabela_cad
#' 
#' @description
#' Esta função lê os dados do RADOC em 2 arquivos e adiciona o resumo na
#' planilha criada pela função tabela_cad
#'
#' @param wb_cad planilha criada pela função tabela_cad.
#' @param pdf_files o caminho para os arquivos contendo os RADOCs.
#'
#' @examples
#'  \dontrun{
#' wb_cad <- "planilha com a tabela cad"
#' pdf_files <- "arquivos pdf com os radocs"
#' resumo_xlsx(wb_cad, pdf_files)
#' }
#' @export
resumo_xlsx <- function(wb_cad, pdf_files) {

   # pegando os dados do docente
   doc <- docente(pdf_files)

   # criando linhas dicionais
   doc[(nrow(doc) + 1), ] <- as.list(c("", ""))
   doc[(nrow(doc) + 1), ] <- as.list(c("", ""))
   doc[(nrow(doc) + 1), ] <- as.list(c("", ""))
   doc[(nrow(doc) + 1), ] <- as.list(c("", ""))

   # criando colunas adicionais
   doc[, "c"] <- as.integer("")
   doc[, "d"] <- as.numeric("")
  
  for (file in pdf_files) {

    # capturando os dados
    df <- pontua(file)

    # criando o título e alterando a coluna de Pontos
    ano <- colnames(df)[[4]]
    titulo  <- paste('Resumo das atividades do ano', ano)

    # capturando o cabecalho
    colnames(df)[4] <- 'Pontos'
    cabecalho <- colnames(df)

    # igualando os nomes
    colnames(doc) <- colnames(df)

    # combinando os dataframes
    df <- rbind(doc, df)

    # extraindo o inicio da tabela de dados
    n_res <- which(stringr::str_detect(df[[1]], paste0("^I")))[[1]]

    # adicionando uma aba
    wb_cad$add_worksheet(sheet = ano)

    # adicionando dos dados na planilha
    wb_cad$add_data(
      sheet = ano,
      df,
      na.strings = '',
      row_names = FALSE,
      col_names = FALSE
    )

    # adicionando o titulo na planilha
    wb_cad$add_data(
      sheet = ano,
      titulo,
      dims = 'B11',
      na.strings = '',
      row_names = FALSE,
      col_names = FALSE
    )

      # adicionando o cabeçalho na planilha
      wb_cad$add_data(
        sheet = ano,
        cabecalho,
        dims = 'A13:D13',
        na.strings = '',
        row_names = FALSE,
        col_names = FALSE
      )

    # editando a fonte da planilha
    wb_cad$add_font(
      sheet =  ano,
      dims = "A1:Z80",
      name = "Calibri",
      size = "16"
    )

    # colocando a primeira coluna docente em negrito
    wb_cad$add_font(
      sheet =  ano,
      dims = paste0("A", 1, ":A", nrow(doc)),
      name = "Calibri",
      size = "16",
      bold = TRUE
    )

    # colocando as linhas destacas em negrito
    for (i in c(11,13)) {
      wb_cad$add_font(
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
      wb_cad$add_fill(
        sheet =  ano,
        dims = cells,
        color = openxlsx2::wb_color(hex = "#d3d3d3")
      )
    }

    # ajustando a largura das colunas
    wb_cad$set_col_widths(
      sheet =  ano,
      cols = 1:2,
      widths = "auto"
    )

       # ajustando a largura das colunas
       wb_cad$set_col_widths(
        sheet =  ano,
        cols = 3:8,
        widths = 8
      )
  }

  # return(paste("O arquivo", output_file, "foi criado"))
  return(wb_cad)
}

