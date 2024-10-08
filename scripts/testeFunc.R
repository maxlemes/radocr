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


Aqui está o código em Markdown para o seu README:

```markdown
# radocr

The `radocr` package provides tools to process PDF files containing faculty RADOCs and generate formatted files (in LaTeX or Excel).

## Installation

To install the package directly from GitHub, you will need `devtools`. If you do not have it installed yet, use:

```r
install.packages("devtools")
devtools::install_github("maxlemes/radocr")
```

## Loading the Package

After installation, load the package with:

```r
library(radocr)
```

## Main Functions

### `resumo_tex` and `resumo_xlsx`

The `resumo_tex` and `resumo_xlsx` functions create a file with a summary of the faculty member's activities contained in the two input RADOCs (in PDF). The difference between them is that the `resumo_tex` function generates a LaTeX (.tex) file, while the `resumo_xlsx` function generates an Excel spreadsheet (.xlsx).

#### Description

The function reads the data from the RADOCs in two PDF files and returns a file with the activities contained in the files.

#### Parameters

- **`pdf_file1`**: Path to the first PDF file containing the RADOC.
- **`pdf_file2`**: Path to the second PDF file containing the RADOC.
- **`output_file`**: Path where the LaTeX file will be saved. If not provided, the file will be saved in the current folder.

#### Details

- If the `output_file` parameter is not specified, the `.tex` file will be saved in the current working directory.

#### Examples

**Example 1:**

```r
pdf_radoc1 <- "path_to_file/radoc1.pdf"
pdf_radoc2 <- "path_to_file/radoc2.pdf"
output_file <- "path_to_destination/filename.tex"
resumo_tex(pdf_radoc1, pdf_radoc2, output_file)
```

**Example 2:**

```r
pdf_radoc1 <- "path_to_file/radoc1.pdf"
pdf_radoc2 <- "path_to_file/radoc2.pdf"
output_file <- "path_to_destination/filename.xlsx"
resumo_xlsx(pdf_radoc1, pdf_radoc2, output_file)
```

### `tabela_cad_tex` and `tabela_cad_xlsx`

The `tabela_cad_tex` and `tabela_cad_xlsx` functions create a file with the Cad Table from two PDF files containing RADOCs. The difference between them is that the `tabela_cad_tex` function generates a LaTeX (.tex) file, while the `tabela_cad_xlsx` function generates an Excel spreadsheet (.xlsx).

#### Description

The function reads the data from the RADOCs in two PDF files and returns a file with the filled Cad Table.

#### Parameters

- **`pdf_file1`**: Path to the first PDF file containing the RADOC.
- **`pdf_file2`**: Path to the second PDF file containing the RADOC.
- **`output_file`**: Path where the LaTeX file will be saved. If not provided, the file will be saved in the current folder.

#### Details

- If the `output_file` parameter is not specified, the `.tex` file will be saved in the current working directory.

#### Examples

**Example 1:**

```r
pdf_radoc1 <- "path_to_file/radoc1.pdf"
pdf_radoc2 <- "path_to_file/radoc2.pdf"
output_file <- "path_to_destination/filename.tex"
tabela_cad_tex(pdf_radoc1, pdf_radoc2, output_file)
```

**Example 2:**

```r
pdf_radoc1 <- "path_to_file/radoc1.pdf"
pdf_radoc2 <- "path_to_file/radoc2.pdf"
output_file <- "path_to_destination/filename.xlsx"
tabela_cad_xlsx(pdf_radoc1, pdf_radoc2, output_file)
```
```