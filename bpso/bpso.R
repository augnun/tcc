bpso <- function(fname,
                 i.swarm,
                 pop,
                 pop_total,
                 cas,
                 casos_total,
                 geo,
                 adj,
                 str,
                 a) {
  swarm <- i.swarm
  nswarm <- nrow(swarm)
  ndim <- ncol(swarm)
  type.bin <- T
  prt <- F
  pmut <- 0.2 # Probabilidade de Mutação
  maxit <- 20 # Máximo de iterações
  
  FF <- do.call(fname, list(swarm, pop, pop_total, cas, casos_total, geo, a))
  pbFF <- FF
  pb <- swarm
  
  gbFF = max(pbFF)
  kk <- which(FF == gbFF)
  gb <- swarm[kk[1], ]
  
  c1 <- 2.05 # ?? 
  c2 <- 2.05 # ?? 
  wmax <- 0.3
  wmin <- 0.2
  vmax <- 2.0
  phi <- c1 + c2
  K <- 2 / (abs(2 - phi - sqrt(phi ^ 2 - 4 * phi)))
  vmax <- 2
  vant = matrix(0, nswarm, ndim)
  
  stop <- F
  it <- 1
  nincr <- 0
  while (!stop) {
    w <- wmax - (it / maxit) * (wmax - wmin)
    #time<-proc.time()
    for (i in 1:nswarm) {
      r1 <- runif(ndim)
      r2 <- runif(ndim)
      if (length(intersect(which(swarm[i, ] == 1), which(gb == 1))) > 0) {
        v <- K * (w * vant[i, ] + c1 * r1 * (pb[i, ] - swarm[i, ]) + c2 * r2 * (gb -
                                                                                  swarm[i, ]))
      } else{
        v <- K * (w * vant[i, ] + c1 * r1 * (pb[i, ] - swarm[i, ]))
      }
      ii <- which(v > 2)
      v[ii] <- 2
      vant[i, ] <- v
      if (type.bin) {
        s <- 1 / (1 + exp(-v))
        x <- runif(ndim)
        new <- rep(0, ndim)
        ii <- which(s == 0.5)
        new[ii] = swarm[i, ii]
        ii <- intersect(which(x < s), which(s != 0.5))
        new[ii] <- 1
      } else{
        new <- swarm[i, ] + v
      }
      swarm[i, ] <- new
    }
    #print(proc.time()-time)
    # mutacao (nao garante conexidade)
    for (i in 1:nswarm) {
      if (runif(1) < pmut) {
        if (runif(1) < 0.5 & length(which(swarm[i, ] == 1) > 1)) {
          # remove region
          k <- sample(which(swarm[i, ] == 1), 1)
          swarm[i, k] = 0
        } else{
          # add region
          viz <- integer(0)
          for (j in which(swarm[i, ] == 1)) {
            viz <- c(viz, which(adj[j, ] == 1))
          }
          viz <- setdiff(viz, which(swarm[i, ] == 1))
          if (length(viz) > 1) {
            k <- sample(viz, 1)
          } else{
            k <- viz
          }
          swarm[i, k] = 1
        }
      }
    }
    #print(proc.time()-time)
    if (prt) {
      # dev.off()
      # dev.new()
      
      # plot(swarm[,1],swarm[,2],"p")
      
      plot(
        str[[1]][[1]][, 1],
        str[[1]][[1]][, 2],
        type = "l",
        xlim = c(0, 460),
        ylim = c(-3, 430),
        yaxt = 'n',
        xaxt='n',
        ann=FALSE,
        frame.plot=FALSE
      )
      for (i in 1:203) {
        shade <- 1 - sum(swarm[, i]) / nswarm
        polygon(str[[i]][[1]][, 1], str[[i]][[1]][, 2], col = rgb(shade, shade, shade))
        if (gb[i] == 1) {
          polygon(str[[i]][[1]][, 1], str[[i]][[1]][, 2], border = rgb(1, 0, 0))
        }
      }
      title(paste("Iteração = ", it))
      # Sys.sleep(0.5)
    }
    
    FF <- do.call(fname, list(swarm, pop, pop_total, cas, casos_total, geo, a))
    ii <- which(FF > pbFF)
    pbFF[ii] <- FF[ii]
    pb[ii, ] <- swarm[ii, ]
    
    if (max(pbFF) > gbFF) {
      gbFF = max(pbFF)
      kk <- which(FF == gbFF)
      gb <- pb[kk[1], ]
      nincr <- 0
    } else{
      nincr <- nincr + 1
    }
    #print(proc.time()-time)
    it <- it + 1
    
    if (it == maxit | nincr == maxit) {
      stop <- T
    }
  }
  gbLLR <- llrpen(t(matrix(gb)), pop, pop_total, cas, casos_total, geo, 0)
  return(list(gb, gbFF, gbLLR, swarm, nincr, it))
}