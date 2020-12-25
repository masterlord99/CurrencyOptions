Eval <- function(simulation,Face,Strike,Premium,Type){
  S <- simulation
  for(i in 1:ncol(simulation)){
    S[,i] <- S[,i]*Face[i]
  }
  ind <- Type == "Call"
  for(i in which(ind)){
    S[,i] <- S[,i] - Strike[i]
  }
  for(i in which(!ind)){
    S[,i] <- -S[,i] + Strike[i]
  }
  Sf <- apply(S, 1, sum)
  S <- apply(S, c(1,2), function(x){max(x,0)})
  for(i in 1:ncol(simulation)){
    S[,i] <- S[,i] - Premium[i]
  }
  S <- apply(S, 1, sum)
  #FP <- mean(Sf)
  vrni <- list(Sf, S)
  names(vrni) <- c("Fu","O")
  return(vrni)
}