tabtibble <- function(tabela){
  #' Converte a tabela em texto em um dataframe
  #'
  #' @param tabela texto com os dados do RADOC do docente
  #'
  #' @return um dataframe com os dados do texto
  #'
  #' @examples
  #' \dontrun{
  #' tabtibble(tabela)
  #' }

  df <- tabela

  df <- stringr::str_replace_all(df, ":", "|")
  df <- stringr::str_replace_all(df,  "\\s{2,}", "|")
  df <- gsub("^$", NA, df)
  df <- tibble::as_tibble(df)

  df <- tidyr::separate_wider_delim(data = df,
                                    cols = value,
                                    delim = '|',
                                    names_sep = '|',
                                    cols_remove = TRUE,
                                    too_few = "align_start"
  )

  for(i in 1:ncol(df)){
    df[[i]] <- stringr::str_replace_all(df[[i]], "^\\s+|\\s+$", "")
  }

  df <- df[!is.na(df[[1]]),]

  colnames(df) <- letters[1:ncol(df)]

  return(df)
}
