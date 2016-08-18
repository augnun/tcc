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
for (i in 1:nrow(dados)) {
  mat_dist_ind[, i] <- head(order(mat_dist[, i]),-1)
}

llr_z <- numeric()

# Scan Circular
for (i in 1:nrow(dados)) {
  zona <- i
  n_z <- dados[i, 4]
  c_z <- dados[i, 1]
  mu_z <- casos_total * (n_z / pop_total)
  ifelse(c_z > mu_z,
         llr_z <-
           c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                               (casos_total - mu_z)),
         llr_z <- 0)
  mat_llr <- cbind(i, llr_z[i])
  while (n_z < pop_total / 2) {
    for (j in mat_dist_ind[i]) {
      zona <<- append(zona, j)
      n_z <- sum(n_z, dados[j, 4])
      c_z <- sum(c_z, dados[j, 1])
      mu_z <- casos_total * (n_z / pop_total)
      if (c_z > mu_z) {
        llr_z <-
          c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                              (casos_total - mu_z))
        mat_llr <- rbind(mat_llr, append(zona, llr_z))
        break()
      }
      else{
          mat_llr <- rbind(mat_llr, append(zona, 0))
        }
      # ifelse(,
      #        mat_llr <- rbind(mat_llr,append(zona, llr_z[i])),
      #        next())
      
    }
  }
}
