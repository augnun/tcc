cluster.max <- data.frame()
llr.max.vet <- data.frame()

for(i in 1:100){
  dados$Casos <- rmultinom(1, pop_total, (casos_total*dados$Pop)/pop_total)
  resultado <- llr.func(dados)
  llr.max.vet <- rbind(llr.max.vet, resultado[which.max(resultado$llr_z),])
}
