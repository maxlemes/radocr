tabela_cad <- function(df) {
  cad <- cadOrig
  cad[,gsub('c.', '', colnames(df)[4])] <-as.numeric(NA)

  aux <- df

  # Localizando os dados do docente no dataframe
  ini <- xts::first(which(stringr::str_detect(aux[[ 1 ]], "^I")))
  end <- xts::first(which(stringr::str_detect(aux[[ 1 ]], "^II")))

  df <- aux[ini:(end-1),]

  cad[1,ncol(cad)] <-sum(df[[ 3 ]])

}
