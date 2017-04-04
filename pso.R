# Inicializa dados
dados <- read.csv("dados_hex/dados.csv", header = T)
mat.adj <- read.table("dados_hex/hex.adj", header = F)
vizinhos <- lapply(mat.adj, function(x)
  which(x == 1, arr.ind = T))

pop_total <- sum(dados$Pop)
casos_total <- sum(dados$Casos)


resultado <- data.frame()
llr_z <- vector(length = nrow(dados) ^ 2)
llr_z.cand <- vector(length = nrow(dados) ^ 2)
zonas <- list(length = nrow(dados) ^ 2)
zonas.cand <- list(length = nrow(dados) ^ 2)
sol.iniciais <- vector(length = nrow(dados)^2)
k = 1

llr.pso <- function(dados) {
  pop_total <- sum(dados$Pop)
  casos_total <- sum(dados$Casos)
  
  # Candidatos com apenas uma região
  for (i in 1:nrow(dados)) {
    zona <- i
    n_z <- dados[zona, 5]
    if (n_z > pop_total / 2) {
      next()
    }
    c_z <- dados[zona, 2]
    mu_z <- casos_total * (n_z / pop_total)
    ifelse(c_z > mu_z,
           llr_z[k] <-
             c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                 (casos_total - mu_z)),
           llr_z[k] <- 0)
    zonas[k] <-  list(zona)
    k <- k + 1
  }
  
  a <- 1
  for (i in 1:nrow(dados)) {
    
    for (j in vizinhos[[i]]) {
      for (l in j)
        zona <- append(i, l)
      n_z <- sum(dados[zona, 5])
      if (n_z > pop_total / 2) {
        k <- k + 1
        next()
      }
      c_z <- sum(dados[zona, 2])
      mu_z <- casos_total * (n_z / pop_total)
      ifelse(
        c_z > mu_z,
        llr_z.cand[a] <-
          c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                              (casos_total - mu_z)),
        llr_z.cand[a] <- 0
      )
      zonas.cand[a] <-  list(zona)
      a <- a + 1
    }
    zona.max <- unlist(zonas.cand[which.max(llr_z.cand)])
    for (n in unique(unlist(vizinhos[unlist(zonas.cand[which.max(llr_z.cand)])]))[! unique(unlist(vizinhos[unlist(zonas.cand[which.max(llr_z.cand)])])) %in% zona]
) {
      zona <- unique(append(zona.max, n))
      n_z <- sum(dados[zona, 5])
      if (n_z > pop_total / 2) {
        k <- k + 1
        next()
      }
      c_z <- sum(dados[zona, 2])
      mu_z <- casos_total * (n_z / pop_total)
      ifelse(
        c_z > mu_z,
        llr_z.cand[a] <-
          c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                              (casos_total - mu_z)),
        llr_z.cand[a] <- 0
      )
      zonas.cand[a] <-  list(zona)
      a <- a + 1
    }
    
  }
  print(c("1"))
  for(n in 1:nrow(dados)){
    if(n %in% zonas.cand)
      sol.iniciais <- append(n)
  }
  return(sol.iniciais)
}
