mtibble <- function(df) {
  #' Função que transforma um tibble em um tibble construction, i.e.,
  #' a função retorna o modo de construção do tibble de entrada
  #'
  #' @param df Um dataframe do tipo tibble
  #'
  #' @return o modo de construção do tibble de entrada
  #' @export
  #'
  #' @examples df <- mtcars[1:3, 1:3]
  #' mtibble(df)

  names <- colnames(df)

  rows <- NULL
  for (j in seq_along(1:ncol(df))) {
    rowj <- NULL
    for (i in seq_along(1:nrow(df))) {
      r <- as.character(df[i,j])
      r <- paste("'", r, "',\n", sep = "")
      rowj <- c(rowj, r)
    }
    rowj <- paste(rowj, collapse = "")
    rowj <- substr(rowj, 1, nchar(rowj) - 2)

    rows  <- c(rows, rowj)
  }

  b <- paste0("'", names, "' = c(\n", rows, "),\n\n")
  b <- paste(b, collapse = "")
  b <- substr(b, 1, nchar(b) - 3)

  bun <- paste0("df <- tibble::tibble(", b, ")")

  cat(bun)
}

