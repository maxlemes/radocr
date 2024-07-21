# radocr

O pacote `radocr` fornece ferramentas para processar arquivos PDF contendo os
RADOCs dos docentes e gerar arquivos formatados (em LaTeX ou e Excel).

## Instalação

Para instalar o pacote diretamente do GitHub, você precisará do `devtools`. Se ainda não o tiver instalado, use:

```r
install.packages("devtools")
devtools::install_github("maxlemes/radocr")
```

## Carregamento do Pacote

Após a instalação, carregue o pacote com:

```r
library(radocr)
```

## Funções Principais

### `resumo_tex` e `resumo_xlsx`

As funções `resumo_tex` e `resumo_xlsx` criam um arquivo com o resumo das
atividades do docente contidas nos dois RADOCs inserido (em PDF). A diferença entre
elas é que enquanto a função `resumo_tex` gera um arquivo LaTeX (.tex) a função
`resumo_xlsx` gera uma planilha em Excel (.xlsx).

#### Descrição

A função lê os dados dos RADOCs em dois arquivos PDF e retorna um arquivo com as
atividades contidas nos arquivos.

#### Parâmetros

- **`pdf_file1`**: Caminho para o primeiro arquivo PDF contendo o RADOC.
- **`pdf_file2`**: Caminho para o segundo arquivo PDF contendo o RADOC.
- **`output_file`**: Caminho onde o arquivo LaTeX será salvo. Se não fornecido, o arquivo será salvo na pasta atual.

#### Detalhes

- Se o parâmetro `output_file` não for especificado, o arquivo `.tex` será salvo no diretório atual de trabalho.

#### Exemplos

**Exemplo 1:**

```r
pdf_radoc1 <- "pasta_do_arquivo/radoc1.pdf"
pdf_radoc2 <- "pasta_do_arquivo/radoc2.pdf"
output_file <- "pasta_de_destino/nome_arquivo.tex"
resumo_tex(pdf_radoc1, pdf_radoc2, output_file)
```

**Exemplo 2:**

```r
pdf_radoc1 <- "pasta_do_arquivo/radoc1.pdf"
pdf_radoc2 <- "pasta_do_arquivo/radoc2.pdf"
output_file <- "pasta_de_destino/nome_arquivo.xlsx"
resumo_xlsx(pdf_radoc1, pdf_radoc2, output_file)
```

### `tabela_cad_tex` e `tabela_cad_xlsx`

As funções `tabela_cad_tex` e `tabela_cad_xlsx` criam um arquivo com a Tabela
Cad a partir de dois arquivos PDF contendo RADOCs. A diferença entre elas é que
enquanto a função `tabela_cad_tex` gera um arquivo LaTeX (.tex) a função
`tabela_cad_xlsx` gera uma planilha em Excel (.xlsx).

#### Descrição da função

A função lê os dados dos RADOCs em dois arquivos PDF e retorna um arquivo com a Tabela Cad preenchida.

#### Parâmetros da função

- **`pdf_file1`**: Caminho para o primeiro arquivo PDF contendo o RADOC.
- **`pdf_file2`**: Caminho para o segundo arquivo PDF contendo o RADOC.
- **`output_file`**: Caminho onde o arquivo LaTeX será salvo. Se não fornecido, o arquivo será salvo na pasta atual.

#### Detalhes da função

- Se o parâmetro `output_file` não for especificado, o arquivo `.tex` será salvo no diretório atual de trabalho.

#### Exemplos da função

**Exemplo 1:**

```r
pdf_radoc1 <- "pasta_do_arquivo/radoc1.pdf"
pdf_radoc2 <- "pasta_do_arquivo/radoc2.pdf"
output_file <- "pasta_de_destino/nome_arquivo.tex"
tabela_cad_tex(pdf_radoc1, pdf_radoc2, output_file)
```

**Exemplo 2:**

```r
pdf_radoc1 <- "pasta_do_arquivo/radoc1.pdf"
pdf_radoc2 <- "pasta_do_arquivo/radoc2.pdf"
output_file <- "pasta_de_destino/nome_arquivo.xlsx"
tabela_cad_xlsx(pdf_radoc1, pdf_radoc2, output_file)
