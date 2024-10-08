radocr: Pacote para Tabelas CAD
================

# radocr

`radocr` é um pacote R que facilita a geração de tabelas CAD a partir de
dados contidos em arquivos PDF de RADOC, que são gerados pelo sistema
SICAD+. O pacote contém duas funções principais:

- `tabela_cad()`: Gera uma tabela CAD em formato Excel.
- `tabela_cad_tex()`: Gera uma tabela CAD em formato LaTeX.

## Instalação

Você pode instalar o pacote diretamente do GitHub:

``` r
# install.packages("devtools")
devtools::install_github("maxlemes/radocr")
```

## Uso

Abaixo estão os exemplos de como usar as funções do pacote `radocr`.

A função `tabela_cad()` gera uma tabela CAD em formato Excel usando
dados contidos em um ou mais arquivos PDF de RADOC gerados pelo SICAD+.

A função `tabela_cad_tex()` gera uma tabela CAD em formato LaTeX usando
dados contidos em um ou mais arquivos PDF de RADOC gerados pelo SICAD+.

## Exemplos

Aqui está um exemplo de como gerar e visualizar a tabela CAD no formato
Excel e no formato LaTeX usando dois arquivos:

``` r
library(radocr)

# Gerar a tabela CAD em formato Excel com dois arquivos RADOC
tabela_cad("radoc1.pdf", "radoc2.pdf")

# Gerar a tabela CAD em formato LaTeX com dois arquivos RADOC
tabela_cad_tex("radoc1.pdf", "radoc2.pdf")
```

## Licença

Este pacote é licenciado sob a licença MIT.
