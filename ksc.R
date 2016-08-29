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

# diag(mat_dist) <- NA
mat_dist_ind <- matrix(ncol = nrow(dados), nrow = nrow(dados) - 1)

# Gerando a matriz de distancias ordenada por indices
for (i in 1:nrow(dados)) {
  mat_dist_ind[, i] <- head(order(mat_dist[, i]),-1)
}


df.llr_z <- data.frame()

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
   df.llr_z <- rbind(df.llr_z,(append(zona, llr_z)))
 }


colnames(df.llr_z) <- c("zonas", "llr")
df.llr_z$zonas <- as.list(df.llr_z$zonas)
tamanho <- 1
resultado <- data.frame()


for(i in 1:nrow(df.llr_z)){
  zona <- i
  # zona <- character()
  for(j in mat_dist_ind[,i]){
    zona <- append(zona, j)
    n_z <- sum(dados[c(noquote(as.vector(zona))), 4])
    if(n_z > pop_total/2){next()}
    else{
    c_z <- sum(dados[c(noquote(as.vector(zona))), 1])
    mu_z <- casos_total*(n_z/pop_total)
    ifelse(c_z > mu_z,
           llr_z <-
             c_z * log(c_z / mu_z) + (casos_total - c_z) * log((casos_total - c_z) /
                                                                 (casos_total - mu_z)),
           llr_z <- 0)
    df.llr_z <- rbind(zona, llr_z)
    # resultado <- cbind(as.list(zona), llr_z)
    df.llr_z <- rbind(df.llr_z, resultado[1:length(resultado) -1])
    # df.llr_z <- rbind(df.llr_z,paste((as.character(zona) llr_z)))
  }
  }
}


  
  

