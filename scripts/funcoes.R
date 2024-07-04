
agrupamento <- function(dt, topini, topend){

  item <- "Item da Resolução"
  # topini <- top[1]
  # topend <- top[2]

  ini <- which(grepl(topini, dt[['description']]))

  if (is.na(topend)){
    end <- nrow(dt)
  } else {
    end <- which(grepl(topend, dt[['description']])) - 1
  }

  df <- dt[ini:end,]

  itens <- which(grepl(item, df[['description']]))

  df <- df[itens,]

  df[['amount']] <- gsub('\\s', '', df[['amount']])

  df[[ncol(df)]] <- as.numeric(df[[ncol(df)]])

  df <- arrange(df, str_rank(amount, numeric = TRUE))

  if (length(df[['description']]) != 0){
    df$ordem <-1:nrow(df)
    df <- df %>%
      group_by(description = amount) %>%
      summarise(amount = n(),
                points = sum(points),
                ordem = min(ordem)) %>%
      mutate(time = 3.2*points) %>%
      relocate(points, .after = last_col()) %>%
      arrange(ordem) %>%
      select(description, amount, time, points)
  }
  return(df)
}

#-------------------------------------------------------------------------------

tabelaCad <- function(df, cad, topCAD){
  df <- df %>% bind_rows(summarise_all(., ~if(
    is.numeric(.)){sum(.)} else {"Total"}))

  cad[which(cad[[1]]== topCAD),][ncol(cad)] <-
    df[[ncol(df)]][nrow(df)]

  return(cad)
}

#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------

# producao <- function(df, da){
#
#   top <- c('II-1-20', 'II-3-9', 'II-3-19.2', 'II-3-21.1', 'II-3-22')
#
#   df <- left_join(df, da, by = join_by(description ==  COD))
#
#   if()
#
#   df[,'Pontuação'] <- df[ ,'amount']*df[,'Pontos']
#
# }

mribble <- function(df) {

  names <- colnames(df)
  names <- sapply("~", paste, names, sep = "")
  names <- as.character(names)
  names <- paste(names, collapse = ", ")
  names <- paste(names, ",\n", sep = "")

  rows <- NULL
  for(i in seq_along(1:nrow(df))) {
    r <- as.character(df[i,])
    r <- paste(r, collapse = ", ")
    r <- paste(r, ",\n", sep = "")
    rows <- c(rows, r)
  }

  last <- rows[length(rows)]
  rows <- rows[-length(rows)]
  last <- substr(last, 1, nchar(last)-3)
  rows <- c(rows, last)

  meat <- c(names, rows)
  meat <- paste(meat, collapse = "")

  bun <- paste("df <- tribble(\n", meat, ")", sep = "")

  cat(bun)
}

mtibble <- function(df) {

  names <- colnames(df)

  # for (j in seq_along(1:ncol(df))){
  #   rows <- NULL
  #   for(i in seq_along(1:nrow(df))) {
  #     r <- as.character(df[i,j])
  #     r <- paste("'", r, "',\n", sep = "")
  #     rows <- c(rows, r)
  #   }
  #   rows <- paste(rows, collapse = "")
  #   rows <- substr(rows, 1, nchar(rows)-2)
  # }

  row1 <- NULL
  for(i in seq_along(1:nrow(df))) {
    r <- as.character(df[i,1])
    r <- paste("'", r, "',\n", sep = "")
    row1 <- c(row1, r)
  }
  row1 <- paste(row1, collapse = "")
  row1 <- substr(row1, 1, nchar(row1)-2)

  row2 <- NULL
  for(i in seq_along(1:nrow(df))) {
    r <- as.character(df[i,2])
    r <- paste("'", r, "',\n", sep = "")
    row2 <- c(row2, r)
  }
  row2 <- paste(row2, collapse = "")
  row2 <- substr(row2, 1, nchar(row2)-2)

  bun <- paste("df <- tibble('",
               names[1], "' = c(\n", row1, "),\n\n", "'",
               names[2], "' = c(\n", row2, ")\n)", sep = "")

  cat(bun)
}
