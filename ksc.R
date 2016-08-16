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

llr_z <- vector()
# Scan Circular
for(i in 1:nrow(dados)){
  n_z <- dados[i, 4]
  c_z <- dados[i, 1]
  mu_z <- casos_total*(n_z/pop_total)
  ifelse(c_z > mu_z,
         llr_z[i] <- c_z*log(c_z/mu_z) + (casos_total - c_z)*log((casos_total - c_z)/(casos_total - mu_z)),
         0)
}

