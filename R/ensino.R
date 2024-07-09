ensino <- function(tabela) {

  ini <- xts::first(which(stringr::str_detect(tabela, '^I ')))
  end <- xts::first(which(stringr::str_detect(tabela, '^II ')))

  df <- tabela[ini:end]

  df <- tabtibble(df)

  itens <- which(grepl("^Item", df[[1]]))

  df <- df[itens,1:4]

  df[[2]] <- stringr::str_replace_all(df[[2]], "\\s", "")


  if (nrow(df) != 0) {
    # converte a ultima coluna em números e eliminas as linhas sem pontos
    df[[ncol(df)]] <- suppressWarnings(as.numeric(df[[ncol(df)]]))
    df <- df[!is.na(df[,ncol(df)]),]

    # ordena as linhas pelo número do item
    df <- dplyr::arrange(df, stringr::str_rank(b, numeric = TRUE))

    # agrupando e somando os itens
    df <- dplyr::group_by(df, a = b)

    df <- dplyr::summarise(df,
                           b = dplyr::n(),
                           d = sum(d))
    df$c <- 3.2 * df$d
    df <- df[,c(1,2,4,3)]

  }

  itens <- which(grepl("^I-3", df[[1]]))
  df[itens,3] <- NA

  return(df)
}
