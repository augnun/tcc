llr.helper <- function(dados, regioes){
  pop_total <- sum(dados$Pop)
  casos_total <- sum(dados$Casos)
  
  llr_z <- vector(length = nrow(dados)^2)
  zonas <- list(length = nrow(dados)^2)
  k = 1
  
  for (i in 1:nrow(dados)) {
    zona <- i
    n_z <- sum(dados[zona, 5])
    if (n_z > pop_total / 2) {
      zona <- zona[1:length(zona) - 1]
      next()
    }
    c_z <- sum(dados[zona, 2])
    mu_z <- casos_total * (n_z / pop_total)
    ifelse(c_z > mu_z,
           llr_z[k] <-
             c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                 (casos_total - mu_z)),
           llr_z[k] <- 0)
    zonas[k] <-  list(zona)
    k <- k + 1
    for (j in dados[, i]) {
      zona <- append(zona, j)
      n_z <- sum(dados[zona, 5])
      if (n_z > pop_total / 2) {
        zona <- zona[1:length(zona) - 1]
        next()
      }
      c_z <- sum(dados[zona, 2])
      mu_z <- casos_total * (n_z / pop_total)
      ifelse(c_z > mu_z,
             llr_z[k] <-
               c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                   (casos_total - mu_z)),
             llr_z[k] <- 0)
      zonas[k] <-  list(zona)
      k <- k + 1
    }
  }
  which.max(llr_z)
  resultado <- list()
  resultado <- list(c(zonas[which.max(llr_z)]),max(llr_z))
  return(resultado)
}