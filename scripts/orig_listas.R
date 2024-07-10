
listas <- list()

# lista com os itens pontuados por tempo da atividade
lista <- c(
  "II-1-20",
  "II-3-9", "II-3-19.2", "II-3-21.1", "II-3-22",
  "III-1-1", "III-1-2", "III-1-3",
  "III-2-1",  "III-2-2",  "III-2-3", "III-2-4", "III-2-5",
  "IV-1-1", "IV-1-2", "IV-1-3", "IV-1-4", "IV-1-5", "IV-1-6", "IV-1-7", "IV-1-8",
  "IV-1-9", "IV-1-10", "IV-1-11", "IV-1-12",  "IV-1-13",  "IV-1-14",  "IV-1-15",
  "IV-2-1", "IV-2-2", "IV-2-3", "IV-2-4", "IV-2-5", "IV-2-5.1", "IV-2-6.1",
  "IV-2-6.2", "IV-2-6.3", "IV-2-6.4", "IV-2-6.5", "IV-2-6.6",
  "IV-3-1", "IV-3-2", "IV-3-3", "IV-3-4", "IV-3-5", "IV-3-6", "IV-3-7", "IV-3-8",
  "IV-3-9", "IV-3-10",  "IV-3-11",  "IV-3-12",  "IV-3-13",  "IV-3-14",
  "IV-3-15",  "IV-3-16",  "IV-3-17",  "IV-3-18",  "IV-3-19",  "IV-3-20.1",
  "IV-3-20.2", "IV-3-20.3", "IV-3-20.4", "IV-3-21", "IV-3-22",
  "IV-4-1", "IV-4-2", "IV-4-3", "IV-4-4", "IV-4-5", "IV-4-6", "V-2-11"
)

listas <- list(lista_tempo = lista)

# lista com os itens limitados a 1 vez
lista <- c(
  'II-1-17.1', 'II-1-17.2', 'II-1-17.3'
)
listas <- append(listas, list(lista1 = lista))

# lista com os itens limitados a 3 pontos
lista <- c(
  'II-4-5'
)
listas <- append(listas, list(lista3 = lista))

# lista com os itens limitados a 4 pontos
lista <- c(
  'II-3-22'
)
listas <- append(listas, list(lista4 = lista))

# lista com os itens limitados a 9 pontos
lista <- c(
  'II-3-21.2', 'II-4-4', 'II-4-6'
)
listas <- append(listas, list(lista9 = lista))

# lista com os itens limitados a 10 pontos
lista <- c(
  'I-3-2',
  'II-1-1.4', 'II-1-3', 'II-1-4.1', 'II-1-4.2', 'II-1-4.3', 'II-1-5.1',
  'II-1-5.2', 'II-1-5.3', 'II-1-9', 'II-3-3', 'II-3-14', 'II-3-10', 'III-1-3',
  'III-2-7', 'III-2-8.3', 'IV-2-2', 'V-3-6'
)
listas <- append(listas, list(lista10 = lista))

# lista com os itens limitados a 12 pontos
lista <- c(
  'III-2-8.2', 'V-1-10', 'V-2-8'
)
listas <- append(listas, list(lista12 = lista))

# lista com os itens limitados a 15 pontos
lista <- c(
  'II-2-14', 'III-2-2', 'III-2-5', 'III-2-6'
)
listas <- append(listas, list(lista15 = lista))

# lista com os itens limitados a 20 pontos
lista <- c(
  'II-1-11', 'II-2-5', 'II-3-12.1', 'II-3-12.2', 'II-3-12.3', 'II-3-13',
  'III-2-8.1', 'V-1-35'
)
listas <- append(listas, list(lista20 = lista))

# lista com os itens limitados a 24 pontos
lista <- c(
  'V-1-9'
)
listas <- append(listas, list(lista24 = lista))

# lista com os itens limitados a 30 pontos
lista <- c(
  'II-3-10', 'II-3-11'
)
listas <- append(listas, list(lista30 = lista))

# lista com os itens limitados a 40 pontos
lista <- c(
  'II-1-9', 'V-1-30'
)
listas <- append(listas, list(lista40 = lista))

# lista com os itens com pontuação atribuida a cada ano de atividade
lista <- c(
  'II-1-20', 'II-3-9', 'II-3-19.2', 'II-3-21.1', 'II-3-22',
  'III-2-1', 'III-2-2', 'III-2-3', 'III-2-4', 'III-2-5',
  'IV-2-1', 'IV-2-2', 'IV-2-4', 'IV-2-5', 'IV-2-5.1', 'IV-2-6.1', 'IV-2-6.2',
  'IV-2-6.3', 'IV-2-6.4', 'IV-2-6.5', 'IV-2-6.6',
  'IV-4-1', 'IV-4-2', 'IV-4-3', 'IV-4-4', 'IV-4-5', 'IV-4-6'
)
listas <- append(listas, list(lista_anual = lista))

# lista com os itens limitados a 10 pontos a cada 150 horas
lista <- c(
  'IV-4-1', 'IV-4-4', 'IV-4-5', 'IV-4-6'
)
listas <- append(listas, list(lista150 = lista))

save(listas, file = 'data/listas.rda')
