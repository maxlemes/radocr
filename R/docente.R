docente <- function (tabela){
  #' Extrai os dados do Docente
  #'
  #' @param tabela texto com os dados do RADOC do docente
  #'
  #' @return um dataframe com os dados do docente
  #'
  #' @examples
  #' \dontrun{
  #' docente(tabela)
  #' }

  df <- tabela

  # Localizando os dados do docente na tabela
  ini <- xts::first(which(stringr::str_detect(tabela, "^Nome")))
  end <- xts::first(which(stringr::str_detect(tabela, "^Cargo")))

  df <- df[ini:end]

  df <- stringr::str_replace_all(df, ":", "|")
  df <- stringr::str_replace_all(df,  "\\s{2,}", "|")
  df <- tibble::as_tibble(df)

  df <- tidyr::separate_wider_delim(data = df,
                                    cols = .data$value,
                                    delim = '|',
                                    names_sep = '|',
                                    cols_remove = TRUE,
                                    too_few = "align_start"
                                    )

  for(i in 1:ncol(df)){
    df[[i]] <- stringr::str_replace_all(df[[i]], "^\\s+|\\s+$", "")
  }



  # Quando há afastamento
  if (df[[1]][2] != "Nenhum registro."){
    inicio <- as.Date(df[[2]][3], format = '%d/%m/%Y')
    termino  <- as.Date(df[[2]][4], format = '%d/%m/%Y')
    reveillon <- as.Date(paste0('31/12/',ano), format = '%d/%m/%Y')


    df[1,2] <- df[2,2]
    df[2,1:2] <- df[2,3:4]
    df[3,1] <- 'Per\u00edodo'
    df[3,2] <- paste(df[3,2], 'a', df[4,2])
    df[4,1] <- 'Dias no ano'
    df[4,2] <- as.character(min(as.numeric(termino + 1 - inicio),
                                as.numeric(reveillon - inicio)))
    df <- df[,1:2]

  } else {
    # quando não há afastamento
   df[1,2] <- df[2,1]
   df <- df[1,1:2]
  }

  colnames(df) <- letters[1:ncol(df)]

  return(df)
}
