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
tabtibble <- function(tabela) { #---  tabtibble  -------------------------------
  #

  # definindo variáveis locais
  value <- NULL

  df <- tabela

  df <- stringr::str_replace_all(df, ":", "|")
  df <- stringr::str_replace_all(df, "\\s{2,}", "|")
  df <- gsub("^$", NA, df)
  df <- tibble::as_tibble(df)

  df <- tidyr::separate_wider_delim(
    data = df,
    cols = value,
    delim = "|",
    names_sep = "|",
    cols_remove = TRUE,
    too_few = "align_start"
  )

  for (i in 1:ncol(df)) {
    df[[i]] <- stringr::str_replace_all(df[[i]], "^\\s+|\\s+$", "")
  }

  df <- df[!is.na(df[[1]]), ]

  colnames(df) <- letters[1:ncol(df)]

  return(df)
}

#' Convertion to title case with lower case for some classes of words.
#'
#' In written Portuguese, when converting to title case, it is not usual
#' to keep in title case some words, like prepositions, conjunctions,
#' articles and some kinds of pronouns.  This functions locates those
#' cases and converts them to lower case.
#'
#' @param texto vector of characters to be converted to title case
#'     but with connector words (one-word prepositions and conjunctions)
#'     and articles (both definite and indefinite) and some pronouns
#'     in lower case.
#'
#' @details
#' The current list of words converted to lower case is:
#' - articles
#'   - o(s), a(s), um, uma(s), uns
#' - pronouns
#'   - me, mim, meu(s), minha(s)
#'   - te, ti, teu(s), tua(s)
#'   - lhe(s), seu(s), sua(s)
#'   - nos, nosso(a)(s)
#'   - vos, vosso(a)(s)
#' - prepositions, their contractions and combinations
#'   - prepositions
#'     - a, ante, até, após, com, contra, de, desde, em, entre, para,
#'       perante, por, sem, sob, sobre, trás
#'   - contractions
#'     - à(s), do(a)(s), no(a)(s), pelo(a)(s), pro(a)(s) (informal language)
#'   - combinations
#'     - ao(s)
#' - conjunctions
#'   - conforme, conquanto, contudo, durante, embora, enquanto, então,
#'     entretanto, exceto, logo, mas, nem, ou, ora, pois, porém, porque,
#'     porquanto, portanto, quando, quanto, que, se, senão, todavia
#'
#' The above list is far from complete or exaustive, mainly due to the absence
#' of the accidental prepositions and conjuntions, which are words that are not
#' originally prepositions or conjunctions, but can play those roles in some contexts,
#' like, for instance _segundo_, which can mean either the numeral _second_ or
#' the prepositional expression _acording to_.
#'
#' @return vector of characters with the same dimension of `string`
#' @export
#' @md
#'
#' @examples
#' to_title_case_pt_br('A VIDA É COMO ELA É')
#'
# You must have package stringr installed.
to_title_case_pt_br <- function(texto) {#---  to_title_case_pt_br  -------------

  texto <- stringr::str_to_title(texto)

  texto <- stringr::str_replace_all(texto, c(
    # articles
    "(.)\\bA(s)?\\b" = "\\1a\\2",
    "(.)\\bO(s)?\\b" = "\\1o\\2",
    "(.)\\bU((m(a(s)?)?)|ns)\\b" = "\\1u\\2",
    # oblique pronouns
    "(.)\\bL(he(s)?)\\b" = "\\1l\\2",
    "(.)\\bM((e(u(s)?)?)|(i(m|(nha(s)?))))\\b" = "\\1m\\2",
    "(.)\\bN(os(s[ao](s)?)?)\\b" = "\\1n\\2",
    "(.)\\bS((e(u(s)?)?)|(ua(s)?))\\b" = "\\1s\\2",
    "(.)\\bT((e(u(s)?)?)|i|(ua(s)?))\\b" = "\\1t\\2",
    "(.)\\bV(os(s[ao](s)?)?)\\b" = "\\1v\\2",
    # prepositions
    "(.)\\bA((o)(s)?|nte|t\u00e9|p\u00f3s)\\b" = "\\1a\\2",
    "(.)\\b\u00c0(s)?\\b" = "\\1\u00e0\\2",
    "(.)\\bC(om|ontra)\\b" = "\\1c\\2",
    "(.)\\bD(((a|o)(s)?)|(e(sde)?))\\b" = "\\1d\\2",
    "(.)\\bE(m|ntre)\\b" = "\\1e\\2",
    "(.)\\bN((a|o)(s)?)\\b" = "\\1n\\2",
    "(.)\\bP(ara|(e((l(a|o)(s)?)|rante))|or)\\b" = "\\1p\\2",
    "(.)\\bS(em|(ob(re)?))\\b" = "\\1s\\2",
    "(.)\\bT(r\u00e1s)\\b" = "\\1t\\2",
    # conjunctions
    "(.)\\bC(on(forme|quanto|tudo))\\b" = "\\1c\\2",
    "(.)\\bD(urante)\\b" = "\\1D\\2",
    "(.)\\bE((mbora|n(quanto|t(\u00e3o|retanto))|xceto)?)\\b" = "\\1e\\2",
    "(.)\\bL(ogo)\\b" = "\\1l\\2",
    "(.)\\bM(as)\\b" = "\\1m\\2",
    "(.)\\bN(em)\\b" = "\\1n\\2",
    "(.)\\bO(u|ra)\\b" = "\\1o\\2",
    "(.)\\bP(o(is|r(\u00e9m|qu(e|anto)|tanto)))\\b" = "\\1p\\2",
    "(.)\\bQ(u(an[dt]o|e))\\b" = "\\1q\\2",
    "(.)\\bS(e(n\u00e3o)?)\\b" = "\\1s\\2",
    "(.)\\bT(odavia)\\b" = "\\1t\\2"
  ))
  return(texto)
}

#' Realiza verificações em arquivos RADOC
#'
#' A função `filesCheck()` faz três testes em um ou mais arquivos PDF de RADOC, 
#' que devem ter sido gerados pelo SICAD+:
#' 
#' 1) Verifica se cada arquivo é um RADOC válido do SICAD+.
#' 2) Verifica se os RADOCs fornecidos pertencem ao mesmo docente.
#' 3) Verifica se os RADOCs fornecidos são de anos diferentes.
#'
#' @param ... Um ou mais caminhos para arquivos PDF contendo dados do RADOC. 
#' Cada PDF deve conter RADOCs gerados pelo SICAD+.
#' 
#' @return Uma mensagem indicando se os arquivos passaram nos testes.
#' 
#' @examples
#' \dontrun{
#' # Exemplo com dois arquivos RADOC
#' filesCheck("radoc1.pdf", "radoc2.pdf")
#' }
filesCheckOLD <- function(...) {#---  filesCheck  ----------------

  files <- c(...)

  aux <- NULL
  ano <- NULL

  # Teste para verificar se os arquivos são RADOCs do SICAD+
  for (i in 1:length(files)) {
    tabela <- readr::read_lines(pdftools::pdf_text(files[i]))
    tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

    if (!grepl("^Sistema de Consulta das Atividades Docente", tabela[1])) {
      warning()(paste(
        "O arquivo", files[i],
        "n\u00e3o parece ser um RADOC gerado pelo SICAD+."
      ))
    }

    # Teste para verificar se os dois RADOCs são do mesmo docente
    aux[i] <- stringr::str_extract(
      tabela[xts::first(which(stringr::str_detect(tabela, "^SIAPE:")))],
      "(^.*[A-Z]?[^\\s](?=\\s\\s))"
    )

    if (i > 1) {
      if (aux[i] != aux[i - 1]) {
        warning("Os RADOCs n\\u00e3o s\\u00e3o do mesmo docente.")
      }
    }

    # Teste para verificar se os dois RADOCs são do mesmo ano
    ind_ano <- tabela[xts::first(
      which(stringr::str_detect(tabela, "^Relat\\u00f3rio do docente"))
    )]
    ano[i] <- as.numeric(stringr::str_sub(
      stringr::str_extract(
        ind_ano,
        "(?<=\\()([^()]*?)(?=\\)[^()]*$)"
      ),
      start = 5
    ))

    if (i > 1) {
      if (ano[i] == ano[i - 1]) {
        warning("Existem RADOCs repetidos (do mesmo ano).")
      }
    }
  }

  return("Tudo certo.")
}
filesCheck <- function(...) {#---  filesCheck  ----------------

  files <- c(...)
  aux <- character()
  ano <- numeric()
  verificar_docente <- TRUE

  for (i in seq_along(files)) {
    tabela <- readr::read_lines(pdftools::pdf_text(files[i]))
    tabela <- stringr::str_replace_all(tabela, "^\\s+|\\s+$", "")

    if (!grepl("^Sistema de Consulta das Atividades Docente", tabela[1])) {
      stop(paste(
        "O arquivo", files[i],
        "n\u00e3o parece ser um RADOC gerado pelo SICAD+."
      ))
    }

    # Tenta localizar a linha com SIAPE:
    siape_linha <- xts::first(which(stringr::str_detect(tabela, "^SIAPE:")))

    if (length(siape_linha) == 0 || is.na(siape_linha)) {
      verificar_docente <- FALSE
      warning(paste("N\u00e3o foi poss\u00edvel verificar o docente no arquivo:", files[i]))
      aux[i] <- NA
    } else {
      extraido <- stringr::str_extract(
        tabela[siape_linha],
        "(^.*[A-Z]?[^\\s](?=\\s\\s))"
      )

      if (is.na(extraido)) {
        verificar_docente <- FALSE
        warning(paste("N\u00e3o foi poss\u00edvel extrair o nome do docente no arquivo:", files[i]))
        aux[i] <- NA
      } else {
        aux[i] <- extraido
        if (i > 1 && aux[i] != aux[i - 1]) {
          stop("Os RADOCs n\u00e3o s\u00e3o do mesmo docente.")
        }
      }
    }

    # Verifica o ano:
    linha_ano <- xts::first(
      which(stringr::str_detect(tabela, "^Relat\u00f3rio do docente"))
    )

    if (is.na(linha_ano)) {
      stop(paste("N\u00e3o foi poss\u00edvel localizar a linha com o ano no arquivo:", files[i]))
    }

    trecho <- stringr::str_extract(
      tabela[linha_ano],
      "(?<=\\()([^()]*?)(?=\\)[^()]*$)"
    )

    if (is.na(trecho)) {
      stop(paste("N\u00e3o foi poss\u00edvel extrair o ano no arquivo:", files[i]))
    }

    ano_extraido <- as.numeric(stringr::str_sub(trecho, start = 5))

    if (is.na(ano_extraido)) {
      stop(paste("Ano inv\u00e1lido no arquivo:", files[i]))
    }

    ano[i] <- ano_extraido

    if (i > 1 && ano[i] == ano[i - 1]) {
      stop("Existem RADOCs repetidos (do mesmo ano).")
    }
  }

  if (!verificar_docente) {
    message("Aviso: N\u00e3o foi poss\u00edvel verificar se os RADOCs s\u00e3o do mesmo docente.")
  }

  return("Tudo certo.")
}
