library("R.matlab")
source("bpso.R")
source("llrpen.R")
source("gerapop.R")

real <- read.table("c4.txt") # cluster verdadeiro
cases <- read.table("cases4.txt") # casos gerados sob Ha

adj <- read.table("hex.adj") # Matriz de Adjascência
pop <- read.table("hex.pop") # Vetor Populacional
geo <- read.table("hex.geo") # Vetor Coordenadas (2d)
# 
# casos_total <- sum(as.numeric(cases[1, ])) # ???
# pop_total <- sum(pop)
# 
# estrutura <- readMat("str.mat")
# estrutura <- estrutura[[1]]
# 
# nsim <- 10
# 
# a <- 1 # forca da penalizacao (a>0)
# 
# # simulacao sob H0 para computar valor critico
# # (quantil 95% da dist. da estatistica de teste sob H0)
# 
# llrh0 <- rep(0, nsim)
# prob <-
#   pop[, 1] / pop_total # sob H0, a probabilidade em cada regiao e proporcional a populacao
# time <- proc.time()
# for (i in 1:nsim) {
#   print(i)
#   cas <- rmultinom(1, casos_total, prob)
#   i.p <- gerapop(203, pop, pop_total, cas, casos_total, geo, a)
#   proc.time()
#   r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
#   llrh0[i] <- r[[3]]
# }
# proc.time() - time
# 
# h095 <- quantile(llrh0, .95)
# 
# sens <- 0
# ppv <- 0
# pow <- 0
# for (i in 1:nsim) {
#   print(i)
#   cas <- as.numeric(cases[i, ])
#   i.p <- gerapop(203, pop, pop_total, cas, casos_total, geo, a)
#   r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
#   detected <- r[[1]]
#   llr <- r[[3]]
#   if (llr > h095) {
#     pow <- pow + 1
#     int <- intersect(which(detected == 1), which(real == 1))
#     sens <- sens + sum(pop[int, ]) / sum(pop[which(real == 1), 1])
#     ppv <- ppv + sum(pop[int, ]) / sum(pop[which(detected == 1), 1])
#   }
# }
# 
# sens <- sens / pow
# ppv <- ppv / pow
# pow <- pow / nsim
# 


simul.bpso.c4 <- function(){
  library("R.matlab")
  source("bpso.R")
  source("llrpen.R")
  source("gerapop.R")
  
  real <- read.table("c4.txt") # cluster verdadeiro
  cases <- read.table("cases4.txt") # casos gerados sob Ha
  
  adj <- read.table("hex.adj") # Matriz de Adjascência
  pop <- read.table("hex.pop") # Vetor Populacional
  geo <- read.table("hex.geo") # Vetor Coordenadas (2d)
  
  casos_total <- sum(as.numeric(cases[1, ])) # ???
  pop_total <- sum(pop)
  
  estrutura <- readMat("str.mat")
  estrutura <- estrutura[[1]]
  
  nsim <- 10
  
  a <- 1 # forca da penalizacao (a>0)
  
  # simulacao sob H0 para computar valor critico
  # (quantil 95% da dist. da estatistica de teste sob H0)
  
  llrh0 <- rep(0, nsim)
  prob <-
    pop[, 1] / pop_total # sob H0, a probabilidade em cada regiao e proporcional a populacao
  time <- proc.time()
  for (i in 1:nsim) {
    print(i)
    cas <- rmultinom(1, casos_total, prob)
    i.p <- gerapop(203, pop, pop_total, cas, casos_total, geo, a)
    proc.time()
    r <- bpso.foreach(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
    llrh0[i] <- r[[3]]
  }
  proc.time() - time
  
  h095 <- quantile(llrh0, .95)
  
  sens <- 0
  ppv <- 0
  pow <- 0
  for (i in 1:nsim) {
    print(i)
    cas <- as.numeric(cases[i, ])
    i.p <- gerapop(203, pop, pop_total, cas, casos_total, geo, a)
    r <- bpso.foreach(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
    detected <- r[[1]]
    llr <- r[[3]]
    if (llr > h095) {
      pow <- pow + 1
      int <- intersect(which(detected == 1), which(real == 1))
      sens <- sens + sum(pop[int, ]) / sum(pop[which(real == 1), 1])
      ppv <- ppv + sum(pop[int, ]) / sum(pop[which(detected == 1), 1])
    }
  }
  
  sens <- sens / pow
  ppv <- ppv / pow
  pow <- pow / nsim
  
  return(c(sens,ppv,pow))
}

library(parallel)
nucleos <- detectCores() - 1


