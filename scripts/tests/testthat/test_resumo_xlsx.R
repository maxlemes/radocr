library(testthat)
library(readxl)
library(radocr)

test_that("resumo_xlsx converte Cria um resumo das atividades docentes", {
  # Caminhos para os arquivos de teste
  pdf_file1 <- system.file("tests/testthat/testdata", "radoc1.pdf", 
  package = "radocr")
  pdf_file2 <- system.file("tests/testthat/testdata", "radoc2.pdf", 
  package = "radocr")
  output_path <- tempfile(fileext = ".xlsx")
  expected_path <- system.file("tests/testthat/testdata", "resumo.xlsx", package = "radocr")

  # Verifique o caminho absoluto
  cat("Current working directory:", getwd(), "\n")
  cat("PDF 1:", pdf_file1, "\n")
  cat("PDF 2:", pdf_file2, "\n")
  cat("Output path:", output_path, "\n")
  cat("Expected TeX path:", expected_tex_path, "\n")

  # Verifique se os caminhos dos arquivos estão corretos
  expect_true(file.exists(pdf_file1))
  expect_true(file.exists(pdf_file2))
  expect_true(file.exists(expected_path))
  
  # Executa a função
  resumo_xlsx(pdf_file1, pdf_file2, output_path)
  
  # Lê o arquivo TeX gerado
  generated_file <-  read_excel(output_path)
  
  # Lê o arquivo XLSX esperado
  expected_file <-  read_excel(expected_path)
  
  # Compara os conteúdos
  expect_equal(generated_file, expected_file)
})
