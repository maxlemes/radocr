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
#' \dontrun{
#' resumo_xlsx(pdf_file1, pdf_file2, output_file)
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
