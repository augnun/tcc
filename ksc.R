# Inicialização dados

arquivos <- list.files("dados_hex", full.names = TRUE)
dados <- lapply(arquivos, read.table, header = FALSE)
dados <- data.frame(dados)
names(dados) <- c("Casos", "X_c", "Y_c", "Pop")

# Matriz de distâncias

mat_dist <- dist(dados[, 2:3])
mat_dist <- as.matrix(mat_dist)

# Vetor ordenado com índices de distâncias para a primeira observação
# mat_dist_ind <-
#   sort(abs(mat_dist[-1, 1] - mat_dist[1, 1]), index.return = TRUE)$ix + 1

mat_dist_ind <- matrix(nrow = nrow(dados), ncol=nrow(dados))
# Laço para calcular matriz de distâncias ordenada com os índices
for(i in 1:length(mat_dist_ind)){
   sort(abs(mat_dist[-i,i]), index.return = TRUE)$ix + 1
}

