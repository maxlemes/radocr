tabelaCad <- function(df, cad, topCAD){
  df <- df %>% bind_rows(summarise_all(., ~if(
    is.numeric(.)){sum(.)} else {"Total"}))

  cad[which(cad[[1]]== topCAD),][ncol(cad)] <-
    df[[ncol(df)]][nrow(df)]

  return(cad)
}
