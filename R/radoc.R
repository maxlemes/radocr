#' radoc <- function(tabela){
#' #' Extrai os dados das atividaddes do docente
#' #'
#' #' @param tabela texto com os dados do RADOC do docente
#' #'
#' #' @return um dataframe com os dados do docente
#' #'
#' #' @examples
#' #' \dontrun{
#' #' docente(tabela)
#' #' }
#'
#'   file <- arquivos[7]
#'
#'   #----- RADOC ------------------------------------------------------------
#'
#'   # transformando a tabela em texto
#'   tabela <- readr::read_lines(pdftools::pdf_text(file))
#'   tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")
#'
#'
#'   #----- I - ATIVIDADES DE ENSINO ----------------------------------------------
#'
#'   df <- ensino(tabela, topicos[1], topicos[7])
#'
#'
#'   #----- II - PRODUÇÃO INTELECTUAL ---------------------------------------------
#'
#'   #----- III - ATIVIDADES DE PESQUISA E DE EXTENSÃO ----------------------------
#'
#'   #----- IV - ATIVIDADES ADMINISTRATIVAS E DE REPRESENTAÇÃO --------------------
#'
#'   #----- V - OUTRAS ATIVIDADES -------------------------------------------------
#'
#'
#' #   agrupamento <- function(dt, topini, topend){
#' #
#' #   item <- "Item da Resolução"
#' #   # topini <- top[1]
#' #   # topend <- top[2]
#' #
#' #   ini <- which(grepl(topini, dt[['description']]))
#' #
#' #   if (is.na(topend)){
#' #     end <- nrow(dt)
#' #   } else {
#' #     end <- which(grepl(topend, dt[['description']])) - 1
#' #   }
#' #
#' #   df <- dt[ini:end,]
#' #
#' #   itens <- which(grepl(item, df[[1]]))
#' #
#' #   df <- df[itens,]
#' #
#' #   df[['amount']] <- gsub('\\s', '', df[['amount']])
#' #
#' #   df[[ncol(df)]] <- as.numeric(df[[ncol(df)]])
#' #
#' #   df <- arrange(df, str_rank(amount, numeric = TRUE))
#' #
#' #   if (length(df[['description']]) != 0){
#' #     df$ordem <-1:nrow(df)
#' #     df <- df %>%
#' #       dplyr::group_by(description = amount) %>%
#' #       dplyr::summarise(amount = n(),
#' #                        points = sum(points),
#' #                        ordem = min(ordem)) %>%
#' #       dplyr::mutate(time = 3.2*points) %>%
#' #       dplyr::relocate(points, .after = last_col()) %>%
#' #       dplyr::arrange(ordem) %>%
#' #       dplyr::select(description, amount, time, points)
#' #   }
#' #   return(df)
#' # }
