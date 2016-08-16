# Inicialização dados

arquivos <- list.files("dados_hex", full.names = TRUE)
dados <- lapply(arquivos, read.table, header = FALSE)
dados <- data.frame(dados)
names(dados) <- c("Casos", "X_c", "Y_c", "Pop")

pop_total <- sum(dados$Pop)
casos_total <- sum(dados$Casos)

# Matriz de distâncias

mat_dist <- dist(dados[, 2:3])
mat_dist <- as.matrix(mat_dist)

diag(mat_dist) = NA
mat_dist_ind <- matrix(ncol = nrow(dados), nrow = nrow(dados) - 1)

# Gerando a matriz de distancias ordenada por indices 
for(i in 1:nrow(dados)){
  mat_dist_ind[,i] <- head(order(mat_dist[,i]), -1)
}


# Scan Circular
for(i in 1:nrow(dados)){
  n_z <- dados[i, 4]
  c_z <- dados[i, 1]

  }

