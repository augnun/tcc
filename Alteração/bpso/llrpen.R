llrpen <- function(z, pop, pop_total, cas, casos_total, geo, a) {
  n <- nrow(z)
  s <- rep(0, n)
  for (i in 1:n) {
    ii <- which(z[i, ] == 1)
    if (length(ii) > 1) {
      dx <- max(geo[ii, 1]) - min(geo[ii, 1])
      
      dy <- max(geo[ii, 2]) - min(geo[ii, 2])
      
      dz <- 2 * dx * dy / (dx + dy)
    } else{
      dz <- 1
    }
    
    cz <- sum(cas[ii])
    pz <- sum(pop[ii, ])
    muz <- casos_total * pz / pop_total
    if (cz > muz) {
      s[i] <-
        (cz * (log(cz) - log(muz)) + (casos_total - cz) * (log(casos_total - cz) - log(casos_total - muz))) -
        a * log(dz)
    } else{
      s[i] <- 0
    }
  }
  return(s)
}