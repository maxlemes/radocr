atividades <- function(dt, topini, topend){

  # topini <- top[1]
  # topend <- top[2]

  item <- "Item da Resolução"
  tempo <- 'Tempo de Atuação'

  ini <- which(grepl(topini, dt[['description']]))

  if (is.na(topend)){
    end <- nrow(dt)
  } else {
    end <- which(grepl(topend, dt[['description']])) - 1
  }

  df <- dt[ini:end,]

  df$aux <- 0

  itens <- which(grepl(item, df[['description']]))
  tempos <- which(str_detect(df[['description']],str_c(c(tempo))))

  for (i in 1:length(itens)){
    df[['aux']][itens[i]] <- as.numeric(df[['amount']][tempos[i]])
  }

  df <- df[itens,]

  df[['amount']] <- gsub('\\s', '', df[['amount']])

  df[['points']] <- as.numeric(df[['points']])

  df <- arrange(df, str_rank(amount, numeric = TRUE))

  if (length(df[['description']]) != 0){
    df$ordem <-1:nrow(df)
    df <- df %>%
      group_by(description = amount) %>%
      summarise(amount = n(),
                points = sum(points),
                time = sum(aux),
                ordem = min(ordem)) %>%
      relocate(points, .after = last_col())%>%
      arrange(ordem) %>%
      select(description, amount, time, points)
  }
  return(df)
}
