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
sol.iniciais <- vector(length = nrow(dados)^2)

llr.pso.novo <- function(dados){
  pop_total <- sum(dados$Pop)
  casos_total <- sum(dados$Casos)
  
  # Obtendo candidatos com apenas uma região
  a <- 1
  for (i in 1:nrow(dados)){
    zona <- i
    n_z <- dados[zona, 5]
    if(n_z > pop_total/2 | length(zona) > nrow(dados)/2){
      a <- a + 1
      next()
    }
    c_z <- dados[zona, 2]
    mu_z <- casos_total * (n_z / pop_total)
    ifelse(c_z > mu_z,
           llr_z[a] <-
             c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                 (casos_total - mu_z)),
           llr_z[a] <- 0)
    zonas[i] <-  list(zona)
    llr_z.cand[i] <- llr_z[a] 
    zonas.cand[i] <- zona
    a <- a + 1
    # Compondo regiões
    for (j in vizinhos[[i]]){
      zona <- append(i,j)
      n_z <- sum(dados[zona, 5])
      if (n_z > pop_total/2 | length(zona) > nrow(dados)/2) {
        a <- a + 1
        next()
      }
      c_z <- sum(dados[zona, 2])
      mu_z <- casos_total * (n_z / pop_total)
      ifelse(
        c_z > mu_z,
        llr_z[a] <-
          c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                              (casos_total - mu_z)),
        llr_z[a] <- 0
      )
      if(llr_z[a] >= llr_z.cand[i]){
        llr_z.cand[i] <- llr_z[a]
        zonas.cand[i] <- list(zona)
        
      }
      zonas[a] <- list(zona)
      a <- a + 1
      # for (n in unique(unlist(vizinhos[unlist(zonas.cand[which.max(llr_z.cand)])]))[! unique(unlist(vizinhos[unlist(zonas.cand[which.max(llr_z.cand)])])) %in% zona]
      # ){
      #   zona <- unique(zonas.cand[i], n)
      #   n_z <- sum(dados[zona, 5])
      #   if (n_z > pop_total/2 | length(zona) > nrow(dados)/2) {
      #     a <- a + 1
      #     next()
      #   }
      #   c_z <- sum(dados[zona, 2])
      #   mu_z <- casos_total * (n_z / pop_total)
      #   ifelse(
      #     c_z > mu_z,
      #     llr_z[a] <-
      #       c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
      #                                                           (casos_total - mu_z)),
      #     llr_z[a] <- 0
      #   )
      #   if(llr_z[a] >= llr_z.cand[i]){
      #     llr_z.cand[i] <- llr_z[a]
      #     zonas.cand[i] <- list(zona)
      #     
      #   }
      #   zonas[a] <- list(zona)
      #   a <- a + 1
      # }
    }
      return(data.frame(cbind(zonas.cand[which(llr_z.cand != 0)]),
                        llr_z.cand[which(llr_z.cand != 0)],row.names = NULL, check.names = F))
      
    }
  }

  



teste <- llr.pso.novo(dados)
colnames(teste) <- c("Zonas Candidatas", "LLR")
