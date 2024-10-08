% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_cad.R
\name{tab_cad}
\alias{tab_cad}
\title{Extrai os dados e retorna um dataframe com a Tabela da CAD}
\usage{
tab_cad(...)
}
\arguments{
\item{pdf_files}{Um ou mais arquivos em PDF contendo RADOCs gerados pelo
SICAD+.}
}
\value{
Um dataframe contendo a Tabela CAD preenchida.
}
\description{
A função \code{tab_cad()} extrai os dados dos RADOC, utiliza o Anexo II para
pontuar
as informações e retorna um dataframe com a Tabela CAD preenchida.
}
\details{
Esta função utiliza duas funções internas: uma para coletar os dados e outra
para pontuar as informações.
}
\examples{
\dontrun{
# Exemplo de uso da função tab_cad
tab_cad(pdf_file1, pdf_file2)
}
}
\keyword{internal}
