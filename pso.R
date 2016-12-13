# Inicializa dados
dados <- read.csv("dados_hex/dados.csv", header = T)
mat.adj <- read.table("dados_hex/hex.adj", header = F)
vizinhos <- lapply(mat.adj, function(x)
  which(x == 1, arr.ind = T))

# unique(unlist(c(vizinhos[1], vizinhos[3])))



llr.pso <- function(dados){
  pop.total <- sum(dados$Pop)
  casos.total <- sum(dados$Casos)
  
}