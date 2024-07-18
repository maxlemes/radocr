#´ Função INTERNA que  lê os dados dos RADOC e cria um dataframe com os dados do 
#' docente
#' 
#' @param pdf_file1,  arquivo PDF com os dados do 1o RADOC
#' @param pdf_file2,  arquivo PDFcom os dados do 2o RADOC
#'
#' @return um dataframe com os dados do docente
#'
#' @examples
#' \dontrun{
#' final_doc(pdf_file1, pdf_file2)
#' }

final_doc <- function(pdf_file1, pdf_file2) {
    # Coletando os dados do docente
    doc <- radocr:::docente(pdf_file1)

    # Verificando os afastamentos
    afast <- cbind(radocr:::afastamentos(pdf_file1), 
                   radocr:::afastamentos(pdf_file2)[,2])
    afast[, 'Total'] <- rowSums(afast[, 2:3])
  
    if (afast[['Total']][1] != 0) {
      aux <- afast[1:2,c(1,4)]
      colnames(aux) <- colnames(doc)
      doc <- rbind(doc, aux)
    } else {
      aux <- tibble::tibble(
        'a' = 'Afastamentos',
        'b' = 'Nenhum Registro'
      )
      doc <- rbind(doc, aux)
    }
}