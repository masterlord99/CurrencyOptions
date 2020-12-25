BootStrap <- function(beta=0.8,lengthRollingVollatilty=22,St.Simulacij=10000,St.Dni.Vnaprej=256,
                      Percent=0.99,STRES=T,Student=T,data,dFree=3,norm=F){
  cat("DATA should be sorted by dates and without missing values!","\n")
  if(length(Percent)!=ncol(data)){
    Percent <- rep(Percent[1],ncol(data))
  }
  if(length(beta)!=ncol(data)){
    beta <- rep(beta[1],ncol(data))
  }
  if(length(STRES)!=ncol(data)){
    STRES <- rep(STRES[1],ncol(data))
  }
  # if(St.Dni.Vnaprej > 128){ ## za več kot pol leta naprej upoštevaj vso zgodovino
  #   Percent <- 1
  # }else{
  #   if(St.Dni.Vnaprej > 66){ ## za obdobje med 3 meseci in pol leta upoštevaj relaxiran smooth
  #     Percent <- 0.999
  #   }
  # }
  #library(doParallel)
  library(Matrix)
  library(QRM)
  library(dplyr)
  try(memory.limit(32000),silent=T)
  try(memory.limit(4090))
  # cores=detectCores()
  # cl <- makeCluster(cores[1])
  resitev <- list()
  
  n <- ncol(data)
  korelacija <- cor(data)
  
  resitev[length(resitev)+1] <- list(korelacija)
  names(resitev)[length(resitev)] <- "Correlation Matrix"
  
  if(!Student){
    korelacija <- nearPD(korelacija)  ## za cholesky razcep rabim poz. definitno matriko, delam aprox.
    blizina <- korelacija$normF
    matrika <- matrix(korelacija$mat,n,n)
    cholesky <- chol(matrika)
    resitev[length(resitev)+1] <- list(blizina)
    names(resitev)[length(resitev)] <- "Matrix F-Norm"
    cat("Finding nearest positive definite matrix. Aprox. error(F-norm) : ",blizina,"\n")
  }
  
  dd <- lengthRollingVollatilty
  cat("Simulating...","\n")
  # clusterExport(cl=cl, list(ls()),
  #               envir=environment())
  for(z in 1:n){
    w <- na.omit(data[,z])
    zadnji <- tail(w,1)
    w <- diff(log(w))
    N <- length(w)
    if(STRES[z]){
      s <- c()
      for(i in 1:(N-dd)){
        o <- c()
        for(j in i:(i+dd)){
          o <- append(o,w[j])
        }
        s <- append(s,sqrt(sum((o - mean(o))^2)/dd))
      }
      om <- quantile(s,beta[z])
      w <- w*(om/sd(w))
    }
    kk1<-mean(w)
    kk2<-var(w)
    vek <- rep(1,N)
    qz1 <- 0
    for(i in length(vek):1){
      vek[i] <- vek[i]*(Percent[z]**qz1)
      qz1 <- qz1 +1
    }
    vek <- vek/ sum(vek)
    sim <- matrix(sample(x=1:N,prob=vek,size=St.Dni.Vnaprej*St.Simulacij,replace=TRUE),nrow = St.Simulacij,byrow = T,ncol=St.Dni.Vnaprej)
    sim <- apply(sim, 1, function(x){
      vz <- w[x]
      ret <- (sum(vz)) - St.Dni.Vnaprej*kk1 - 0.5*St.Dni.Vnaprej*kk2
      if(norm){
        ret <- exp(ret)
      }else{
        ret <- exp(ret)*zadnji 
      }
      return(ret)
    })
    if(z==1){
      simulacije <- data.frame(Sim=sim)
    }
    else{
      simulacije <- cbind(simulacije, sim)
    }
  }
  simulacije <- as.matrix(simulacije)
  
  if(!Student){
    popravljam <- matrix(rnorm(n*St.Simulacij),nrow=St.Simulacij,ncol=n,byrow = T)
    popravljam <- popravljam %*% cholesky
    popravljam <- apply(popravljam,2,function(x) pnorm(x))
    cat("Correlating simulation with Gaussian copula...","\n")
  }else{
    cat("Correlating simulation with Student copula...","\n")
    popravljam <- QRM::rcopula.t(n=St.Simulacij,df = dFree ,Sigma =korelacija)
  }
  
  for(i in 1:dim(simulacije)[2]){
    popravljam[,i] <- quantile(simulacije[,i],popravljam[,i]) %>% as.numeric()
  }
  popravljam <- as.matrix(popravljam)
  colnames(popravljam) <- colnames(data)
  
  resitev[length(resitev)+1] <- list(popravljam)
  names(resitev)[length(resitev)] <- "Results"
  cat("Done!","\n")
  #stopCluster(cl)
  return(resitev)
}

