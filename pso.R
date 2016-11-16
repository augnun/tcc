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

# resultado <- 0
# vizinhos <- list()
# 
# for (i in vizinhos$col){
# 
#   for (j in subset(vizinhos, vizinhos$col == i)$row){
#     zona <- append(i, j)
#     n_z <- sum(dados[zona, 4])
#     if(n_z > pop_total/2){
#       zona <- zona[1:length(zona)-1]
#       next()
#     }
#     c_z <- sum(dados[zona, 1])
#     mu_z <- casos_total * (n_z / pop_total)
#     ifelse(c_z > mu_z,
#            llr_z <-
#              c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
#                                                                  (casos_total - mu_z)),
#            llr_z <- 0)
#     if(llr_z > resultado){
#     resultado <-   llr_z
#     zonas <- zona
#     }
#     else{  next()}
#   }
#     for (j in subset(vizinhos, vizinhos$col == zonas[-1])$row){
#       zona <- append(zona, j)
#       n_z <- sum(dados[zona, 4])
#       if(n_z > pop_total/2){
#         zona <- zona[1:length(zona)-1]
#         next()
#       }
#       c_z <- sum(dados[zona, 1])
#       mu_z <- casos_total * (n_z / pop_total)
#       ifelse(c_z > mu_z,
#              llr_z <-
#                c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
#                                                                    (casos_total - mu_z)),
#              llr_z <- 0)
#       if(llr_z > resultado){
#         resultado <-   llr_z
#         zonas <- zona
#       }
#   }
# }

# vizinhos <- lapply(mat.adj, function(x)
#   which(x == 1, arr.ind = T))
# resultado <- data.frame()
# zonas_candidatas <- data.frame()
# 
# for (i in 1:nrow(dados)) {
#   zona <- i
#   n_z <- dados[i, 4]
#   if (n_z > pop_total / 2) {
#     next()
#   }
#   c_z <- dados[i, 1]
#   mu_z <- casos_total * (n_z / pop_total)
#   ifelse(c_z > mu_z,
#          llr_z <-
#            c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
#                                                                (casos_total - mu_z)),
#          llr_z <- 0)
#   zonas_candidatas <- rbind(zonas_candidatas, cbind(zona, llr_z))
#   for (j in 1:nrow(dados)) {
#     for (k in 1:length(vizinhos[[j]])) {
#       zona <- unique(append(zona, vizinhos[[j]][k]))
#       n_z <- sum(dados[zona, 4])
#       if (n_z > pop_total  /  2) {
#         next()
#       }
#       c_z <- sum(dados[zona, 1])
#       mu_z <- casos_total * (n_z / pop_total)
#       if (c_z > mu_z) {
#         llr_z <-
#           c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
#                                                               (casos_total - mu_z))
#         zonas_candidatas <- data.frame(zonas = c(zona), llr_z)
#         
#       }
#       else{
#         llr_z <- append(llr_z, 0)
#         zonas_candidatas <-
#           rbind(zonas_candidatas, cbind(list(zona), llr_z))
#       }
#       
#     }
#     
#   }
# }


vizinhos <- lapply(mat.adj, function(x)
  which(x == 1, arr.ind = T))


for(i in 1:nrow(dados)){
  zona_inicial <- i
  n_z <- dados[i, 4]
  if(n_z > pop_total/2){ next()}
  c_z <- dados[i, 1]
  mu_z <- casos_total * (n_z / pop_total)
  ifelse(c_z > mu_z,
         llr_z <-
           c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                               (casos_total - mu_z)),
         llr_z <- 0)
  for(j in vizinhos[[i]]){
    zona <- append(zona_inicial, j)
  }
  
  
}

