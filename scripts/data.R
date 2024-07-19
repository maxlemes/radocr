#' Tabela do Anexo II da Resolução CONSUNI 18/2017
#'
#' Contém a Tabela do Anexo II com suas respectivas pontuações
#'
#' @format ## 'anexoII'
#'  A tibble: 232 × 3
#'  \describe{
#'  \item{Item}{Contém o código da atividade.}
#'  \item{Descrição}{Contém a descrição da atividade.}
#'  \item{Pontos}{Contém a pontuação da atividade.}
#'  }
#'
#'  @source https://sistemas.ufg.br/consultas_publicas/resolucoes/arquivos/Resolucao_CONSUNI_2017_0018.pdf
"anexoII"

#' Tabela de Pontuação da CAD
#'
#' Contém a Tabela de pontuação da CAD, com os itens a serem pontuados, separados por grupos
#'
#' @format ## 'orig_cad'
#' A tibble: 24 × 2
#' \describe{
#' \item{Item}{Contém o Item da ser avaliado}
#' \item{Avaliação de desempenho para Progressão}{Contem  uma breve descrição do item a ser avaliado}
#' }
#'
#'  @source https://sistemas.ufg.br/consultas_publicas/resolucoes/arquivos/Resolucao_CONSUNI_2017_0018.pdf
"orig_cad"

#' Tópicos
#'
#' Contém a lista de tópicos para a localização dos dados no SICAD+
#'
#' @format ## topicos
#'
#' \describe{
#' \item{"I - ATIVIDADES DE ENSINO"}{}
#' \item{"I-1 Ensino Básico"}{}
#' \item{"I-1 Graduação"}{}
#' \item{"..."}{}
#' }
#'
'topicos'

#' Listas
#'
#' Contém as listas de tópicos com pontuações diferenciadas
#'
#' @format ## listas
#'
#' \describe{
#' \item{"lista_tempo"}{lista com os itens pontuados por tempo da atividade}
#' \item{"lista1"}{lista com os itens limitados a 1 evento por ano}
#' \item{"lista3"}{lista com os itens limitados a 3 pontos por ano}
#' \item{"lista4"}{lista com os itens limitados a 4 pontos por ano}
#' \item{"lista9"}{lista com os itens limitados a 9 pontos por ano}
#' \item{"lista10"}{lista com os itens limitados a 10 pontos por ano}
#' \item{"lista12"}{lista com os itens limitados a 12 pontos por ano}
#' \item{"lista15"}{lista com os itens limitados a 15 pontos por ano}
#' \item{"lista20"}{lista com os itens limitados a 20 pontos por ano}
#' \item{"lista24"}{lista com os itens limitados a 24 pontos por ano}
#' \item{"lista30"}{lista com os itens limitados a 30 pontos por ano}
#' \item{"lista40"}{lista com os itens limitados a 40 pontos por ano}
#' \item{"lista_anual"}{lista com os itens pontuodos por cada ano de atividade}
#' \item{"lista150"}{lista com os itens limitados a 10 pontos a cada 150 horas}
#' }
#'
'listas'

