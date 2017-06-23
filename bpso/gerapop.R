#' Title Gera População
#'
#' @param n dimensão do vetor binário
#' @param pop população na região descrita pelo vetor binário
#' @param pop_total total populacional
#' @param cas casos na região descrita pelo vetor binário
#' @param casos_total total de casos
#' @param geo estrutura do mapa
#' @param a penalização
#'
#' @return cluster que será inicializado como solução no BPSO
gerapop <- function(n, pop, pop_total, cas, casos_total, geo, a) {
  len <- length(cas)
  i.p <- matrix(rep(0, n * len), ncol = len, nrow = n)
  LL <- rep(0, n)
  for (i in 1:n) {
    z <- matrix(rep(0, len), nrow = 1, ncol = len)
    z[i] = 1
    LL[i] <- llrpen(z, pop, pop_total, cas, casos_total, geo, a)
    crit <- F
    v <- which(adj[i,] == 1)
    while (!crit) {
      nv <- length(v)
      Lv <- rep(0, nv)
      for (j in 1:nv) {
        zz <- z
        zz[v[j]] <- 1
        Lv[j] <-
          llrpen(zz, pop, pop_total, cas, casos_total, geo, a)
      }
      if (max(Lv) > LL[i]) {
        LL[i] <- max(Lv)
        k <- which(Lv == max(Lv))
        if (length(k) > 1) {
          k <- k[1]
        }
        z[v[k]] <- 1
        vc <- which(adj[v[k]] == 1)
        v <- c(v, vc)
        v <- setdiff(v, which(z == 1))
      }
      else{
        crit <- T
        i.p[i,] <- z
      }
      if (sum(z) >= len / 4) {
        crit <- T
        i.p[i,] <- z
      }
    }
  }
  return(i.p)
}