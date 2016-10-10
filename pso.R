arquivos <- list.files("dados_hex", full.names = TRUE)
dados <- lapply(arquivos, read.table, header = FALSE)
dados <- data.frame(dados)
names(dados) <- c("Casos", "X_c", "Y_c", "Pop")

pop_total <- sum(dados$Pop)
casos_total <- sum(dados$Casos)

mat_dist <- dist(dados[, 2:3])
mat_dist <- as.matrix(mat_dist)

diag(mat_dist) <- NA
mat_dist_ind <- matrix(ncol = nrow(dados), nrow = nrow(dados) - 1)

# Gerando a matriz de distancias ordenada por indices
for (i in 1:nrow(dados)) {
  mat_dist_ind[, i] <- head(order(mat_dist[, i]), -1)
}


# Matriz de adjacências 
mat.adj <- read.csv("hex.adj", sep = "", header=FALSE)

resultado <- 0
vizinhos <- data.frame(which(mat.adj == 1, arr.ind = T))

for (i in vizinhos$col){
  
  for (j in subset(vizinhos, vizinhos$col == i)$row){
    zona <- append(i, j)
    n_z <- sum(dados[zona, 4])
    if(n_z > pop_total/2){
      zona <- zona[1:length(zona)-1]
      next()
    }
    c_z <- sum(dados[zona, 1])
    mu_z <- casos_total * (n_z / pop_total)
    ifelse(c_z > mu_z,
           llr_z <-
             c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                 (casos_total - mu_z)),
           llr_z <- 0)
    if(llr_z > resultado){
    resultado <-   llr_z
    zonas <- zona
    }
    else{  next()}
  }
    for (j in subset(vizinhos, vizinhos$col == zonas[-1])$row){
      zona <- append(zona, j)
      n_z <- sum(dados[zona, 4])
      if(n_z > pop_total/2){
        zona <- zona[1:length(zona)-1]
        next()
      }
      c_z <- sum(dados[zona, 1])
      mu_z <- casos_total * (n_z / pop_total)
      ifelse(c_z > mu_z,
             llr_z <-
               c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                   (casos_total - mu_z)),
             llr_z <- 0)
      if(llr_z > resultado){
        resultado <-   llr_z
        zonas <- zona
      }
  }
  
}