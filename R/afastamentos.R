afastamentos <- function (tabela){
  #' Afastamentos
  #'
  #' @param tabela texto com os dados do RADOC do docente
  #'
  #' @return um dataframe com os dados do afastamento do docente
  #'
  #' @examples
  #' \dontrun{
  #' afastamentos(tabela)
  #' }

  df <- tabela

  ini <- xts::first(which(stringr::str_detect(df, "^Afastamentos")))

  df <- df[(ini):(ini + 3)]

  df <- tabtibble(df)

  # Quando há afastamento
  if (df[[1]][2] != "Nenhum registro."){
    newyear <- as.Date(paste0('01/01/',ano), format = '%d/%m/%Y')
    inicio <- as.Date(df[[2]][3], format = '%d/%m/%Y')
    termino  <- as.Date(df[[2]][4], format = '%d/%m/%Y')
    reveillon <- as.Date(paste0('31/12/',ano), format = '%d/%m/%Y')


    df[1,2] <- df[2,2]
    df[2,1:2] <- df[2,3:4]
    df[3,1] <- 'Per\u00edodo'
    df[3,2] <- paste(df[3,2], 'a', df[4,2])
    df[4,1] <- 'Dias no ano'
    df[4,2] <- if (inicio > newyear){
      as.character(min(as.numeric(termino + 1 - inicio),
                       as.numeric(reveillon - inicio)))
    } else{
      as.character(min(as.numeric(termino + 1 - newyear),
                       as.numeric(reveillon - newyear)))
    }

    df <- df[,1:2]

  } else {
    # quando não há afastamento
   df[1,2] <- df[2,1]
   df <- df[1,1:2]
  }

  return(df)
}
