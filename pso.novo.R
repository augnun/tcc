library(dplyr)

# Inicialização dos dados
dados <- read.csv("dados_hex/dados.csv", header = T)

# Matriz de adjacência
mat.adj <- read.table("dados_hex/hex.adj", header = F)

# Listas de vizinhos
vizinhos <- lapply(mat.adj, function(x)
  which(x == 1, arr.ind = T))

# Estruturas de dados
llr_z <- vector(length = nrow(dados) ^ 2)
llr_z.cand <- vector(length = nrow(dados) ^ 2)
zonas <- list(length = nrow(dados) ^ 2)
zonas.cand <- list(length = nrow(dados) ^ 2)
sol.iniciais <- vector(length = nrow(dados) ^ 2)

llr.pso.novo <-
  function(dados,
           metodo = 1,
           resultado = c("todos")) {
    pop_total <- sum(dados$Pop)
    casos_total <- sum(dados$Casos)
    
    if (metodo == 1) {
      for (i in 1:nrow(dados)) {
        stop = 0
        zona <- i
        n_z <- sum(dados[zona, 5])
        if (n_z > pop_total / 2 | length(zona) > nrow(dados) / 2) {
          next()
        }
        c_z <- sum(dados[zona, 2])
        mu_z <- casos_total * (n_z / pop_total)
        ifelse(c_z > mu_z,
               llr_z <-
                 c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                     (casos_total - mu_z)),
               llr_z <- 0)
        llr_z.cand[i] <- llr_z
        zonas.cand[i] <- list(zona)
        
        
        
        
        while (stop == 0) {
          for (j in 1:length(vizinhos[[i]])) {
            zona <- unique(append(zona, unlist(vizinhos[zona])[j]))
            n_z <- sum(dados[zona, 5])
            if (n_z > pop_total / 2 |
                length(zona) > nrow(dados) / 2) {
              next()
            }
            c_z <- sum(dados[zona, 2])
            mu_z <- casos_total * (n_z / pop_total)
            ifelse(
              c_z > mu_z,
              llr_z <-
                c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                    (casos_total - mu_z)),
              llr_z <- 0
            )
            if (llr_z > llr_z.cand[i]) {
              llr_z.cand[i] <- llr_z
              zonas.cand[i] <- list(zona)
            }
            else{
              length(zona) <- length(zona) - 1
              if (length(zona) > 1 & j < length(zona)) {
                next()
              }
              if (length(zona) == 1 &
                  j == length(unlist(vizinhos[zona]))) {
                stop = 1
                break()
              }
              next()
            }
            
          }
        }
        
      }
      if (resultado == c("maximo")) {
        resultado <-
          data.frame(cbind(zonas.cand[which.max(llr_z.cand)], llr_z.cand[which.max(llr_z.cand)]))
        colnames(resultado) <- c("Zona", "LLR")
        return(resultado)
      }
      if (resultado == c("todos")) {
        resultado <-
          data.frame(cbind(zonas.cand[which(llr_z.cand != 0)], llr_z.cand[which(llr_z.cand != 0)]),
                     row.names = NULL)
        colnames(resultado) <- c("Zona", "LLR")
        return(resultado)
      }
    }
    
    
    if (metodo == 2) {
      for (i in 1:nrow(dados)) {
        # Obtendo candidatos com apenas uma região
        zona <- i
        n_z <- sum(dados[zona, 5])
        if (n_z > pop_total / 2 | length(zona) > nrow(dados) / 2) {
          next()
        }
        c_z <- sum(dados[zona, 2])
        mu_z <- casos_total * (n_z / pop_total)
        ifelse(c_z > mu_z,
               llr_z <-
                 c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                     (casos_total - mu_z)),
               llr_z <- 0)
        llr_z.cand[i] <- llr_z
        zonas.cand[i] <- list(zona)
        # Compondo regiões
        stop = 0
        zona <- append(i, vizinhos[[i]])
        while (stop == 0) {
          n_z <- sum(dados[zona, 5])
          if (n_z > pop_total / 2 | length(zona) > nrow(dados) / 2) {
            a <- a + 1
            next()
          }
          c_z <- sum(dados[zona, 2])
          mu_z <- casos_total * (n_z / pop_total)
          ifelse(c_z > mu_z,
                 llr_z <-
                   c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                       (casos_total - mu_z)),
                 llr_z <- 0)
          if (llr_z > llr_z.cand[i]) {
            llr_z.cand[i] <- llr_z
            zonas.cand[i] <- list(zona)
            zona <- unique(append(i, unique(unlist(
              vizinhos[c(zona)]
            ))))
            n_z <- sum(dados[zona, 5])
            if (n_z > pop_total / 2 |
                length(zona) > nrow(dados) / 2) {
              a <- a + 1
              next()
            }
            c_z <- sum(dados[zona, 2])
            mu_z <- casos_total * (n_z / pop_total)
            ifelse(
              c_z > mu_z,
              llr_z <-
                c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                    (casos_total - mu_z)),
              llr_z <- 0
            )
            if (llr_z > llr_z.cand[i]) {
              llr_z.cand[i] <- llr_z
              zonas.cand[i] <- list(zona)
            }
            if (length(zona) > nrow(dados) / 2) {
              next()
            }
            if (length(zona) - 1 > 1) {
              length(zona) = length(zona) - 1
            }
            else{
              stop = 1
            }
            
            next()
          }
          if (length(zona) > nrow(dados) / 2) {
            next()
          }
          if (length(zona) - 1 > 1) {
            length(zona) = length(zona) - 1
          }
          else{
            stop = 1
          }
        }
      }
      return(data.frame(cbind(c(zonas.cand), llr_z.cand)))
    }
    
  }





candidatas <- llr.pso.novo(dados, resultado = c("todos"))
head(arrange(candidatas, desc(as.numeric(LLR))), n = 10)

Reduce(intersect, head(arrange(candidatas, desc(as.numeric(LLR))), n = 10))


