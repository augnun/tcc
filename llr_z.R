# Exemplo de chamada
# llr_z(c(113, 94, 114, 81, 110, 86),
#       rep(1000,6),
#       c(450.8577, 417.5139, 378.7812, 347.2622, 312.0788, 275.8123),
#       c(420.3071, 420.1352, 420.5152, 420.2614, 419.0585, 419.8377))
# 

llr_z <- function(dados, tipo = "maximo" ){
  pop_total <- sum(dados$Pop)
  casos_total <- sum(dados$Casos)
  
  # Matriz de distâncias
  
  mat_dist <- dist(dados[, 2:3])
  mat_dist <- as.matrix(mat_dist)
  
  diag(mat_dist) <- NA
  mat_dist_ind <- matrix(ncol = nrow(dados), nrow = nrow(dados) - 1)
  
  # Gerando a matriz de distancias ordenada por indices
  for (i in 1:nrow(dados)) {
    mat_dist_ind[, i] <- head(order(mat_dist[, i]), -1)
  }
  
  k = 1
  resultado <- data.frame(zona = rep(c(0), nrow(dados)^2), llr_z= rep(NA, nrow(dados)^2))
if(tipo == "todos"){
  for(i in 1:ncol(mat_dist_ind)){
    zona <- i
    for(j in mat_dist_ind[,i]){
      zona <- append(zona,j)
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
      resultado <- rbind(resultado, cbind(list(zona), llr_z))
      
    }
  }
  return(resultado)
}
  if(tipo == "maximo"){
    for(i in 1:ncol(mat_dist_ind)){
      zona <- i
      for(j in mat_dist_ind[,i]){
        zona <- append(zona,j)
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
        resultado[k,1] <- c(zona)
        resultado[k,2] <- llr_z
        k = k+1
        
      }
    }
    return(resultado[which.max(resultado$llr_z),])
    }
}

