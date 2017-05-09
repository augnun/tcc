library("R.matlab")
source("bpso.R")
source("llrpen.R")
source("gerapop.R")


##### 
#CASO 1
real <- read.table("c1.txt") # cluster verdadeiro
cases <- read.table("cases1.txt") # casos gerados sob Ha

adj <- read.table("hex.adj") # Matriz de Adjascência
pop <- read.table("hex.pop") # Vetor Populacional
geo <- read.table("hex.geo") # Vetor Coordenadas (2d)

casos_total <- sum(as.numeric(cases[1, ])) # ???
pop_total <- sum(pop)

estrutura <- readMat("str.mat")
estrutura <- estrutura[[1]]

nsim <- 10000

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
  r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
  llrh0[i] <- r[[3]]
}
proc.time() - time

h095.1 <- quantile(llrh0, .95)

sens1 <- 0
ppv1 <- 0
pow1 <- 0
for (i in 1:nsim) {
  print(i)
  cas <- as.numeric(cases[i, ])
  i.p <- gerapop(203, pop, pop_total, cas, casos_total, geo, a)
  r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
  detected <- r[[1]]
  llr <- r[[3]]
  if (llr > h095) {
    pow1 <- pow1 + 1
    int <- intersect(which(detected == 1), which(real == 1))
    sens1 <- sens1 + sum(pop[int, ]) / sum(pop[which(real == 1), 1])
    ppv1 <- ppv1 + sum(pop[int, ]) / sum(pop[which(detected == 1), 1])
  }
}

sens1 <- sens1 / pow1
ppv1 <- ppv1 / pow1
pow1 <- pow1 / nsim



######## 
#CASO 2
real <- read.table("c2.txt") # cluster verdadeiro
cases <- read.table("cases2.txt") # casos gerados sob Ha

adj <- read.table("hex.adj") # Matriz de Adjascência
pop <- read.table("hex.pop") # Vetor Populacional
geo <- read.table("hex.geo") # Vetor Coordenadas (2d)

casos_total <- sum(as.numeric(cases[1, ])) # ???
pop_total <- sum(pop)

estrutura <- readMat("str.mat")
estrutura <- estrutura[[1]]

nsim <- 10000

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
  r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
  llrh0[i] <- r[[3]]
}
proc.time() - time

h095.2 <- quantile(llrh0, .95)

sens2 <- 0
ppv2 <- 0
pow2 <- 0
for (i in 1:nsim) {
  print(i)
  cas <- as.numeric(cases[i, ])
  i.p <- gerapop(203, pop, pop_total, cas, casos_total, geo, a)
  r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
  detected <- r[[1]]
  llr <- r[[3]]
  if (llr > h095) {
    pow2 <- pow2 + 1
    int <- intersect(which(detected == 1), which(real == 1))
    sens2 <- sens2 + sum(pop[int, ]) / sum(pop[which(real == 1), 1])
    ppv2 <- ppv2 + sum(pop[int, ]) / sum(pop[which(detected == 1), 1])
  }
}

sens2 <- sens2 / pow2
ppv2 <- ppv2 / pow2
pow2 <- pow2 / nsim


######## 
#CASO 3
real <- read.table("c3.txt") # cluster verdadeiro
cases <- read.table("cases3.txt") # casos gerados sob Ha

adj <- read.table("hex.adj") # Matriz de Adjascência
pop <- read.table("hex.pop") # Vetor Populacional
geo <- read.table("hex.geo") # Vetor Coordenadas (2d)

casos_total <- sum(as.numeric(cases[1, ])) # ???
pop_total <- sum(pop)

estrutura <- readMat("str.mat")
estrutura <- estrutura[[1]]

nsim <- 10000

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
  r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
  llrh0[i] <- r[[3]]
}
proc.time() - time

h095.3 <- quantile(llrh0, .95)

sens3 <- 0
ppv3 <- 0
pow3 <- 0
for (i in 1:nsim) {
  print(i)
  cas <- as.numeric(cases[i, ])
  i.p <- gerapop(203, pop, pop_total, cas, casos_total, geo, a)
  r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
  detected <- r[[1]]
  llr <- r[[3]]
  if (llr > h095) {
    pow3 <- pow3 + 1
    int <- intersect(which(detected == 1), which(real == 1))
    sens3 <- sens3 + sum(pop[int, ]) / sum(pop[which(real == 1), 1])
    ppv3 <- ppv3 + sum(pop[int, ]) / sum(pop[which(detected == 1), 1])
  }
}

sens3 <- sens3 / pow3
ppv3 <- ppv3 / pow3
pow3 <- pow3 / nsim



######## 
#CASO 4
real <- read.table("c4.txt") # cluster verdadeiro
cases <- read.table("cases4.txt") # casos gerados sob Ha

adj <- read.table("hex.adj") # Matriz de Adjascência
pop <- read.table("hex.pop") # Vetor Populacional
geo <- read.table("hex.geo") # Vetor Coordenadas (2d)

casos_total <- sum(as.numeric(cases[1, ])) # ???
pop_total <- sum(pop)

estrutura <- readMat("str.mat")
estrutura <- estrutura[[1]]

nsim <- 10000

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
  r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
  llrh0[i] <- r[[3]]
}
proc.time() - time

h095.4 <- quantile(llrh0, .95)

sens4 <- 0
ppv4 <- 0
pow4 <- 0
for (i in 1:nsim) {
  print(i)
  cas <- as.numeric(cases[i, ])
  i.p <- gerapop(203, pop, pop_total, cas, casos_total, geo, a)
  r <- bpso(llrpen, i.p, pop, pop_total, cas, casos_total, geo, adj, estrutura, a)
  detected <- r[[1]]
  llr <- r[[3]]
  if (llr > h095) {
    pow4 <- pow4 + 1
    int <- intersect(which(detected == 1), which(real == 1))
    sens4 <- sens4 + sum(pop[int, ]) / sum(pop[which(real == 1), 1])
    ppv4 <- ppv4 + sum(pop[int, ]) / sum(pop[which(detected == 1), 1])
  }
}

sens4 <- sens4 / pow4
ppv4 <- ppv4 / pow4
pow4 <- pow4 / nsim