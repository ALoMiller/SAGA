  get.multinomial.props <- function(agedata,big.len,big.age,ref.age){
    agedata$AGE <- relevel(as.factor(agedata$AGE), ref=ref.age)  # relevel and make categorical
    my.levels <- as.numeric(levels(agedata$AGE))
    n.levels <- length(my.levels)
    mn <- multinom(AGE ~ LENGTH, data=agedata)
    Parameters <- coefficients(mn)   # Parameters for multinomial key
    newdata <- data.frame(cbind(LENGTH = 1:big.len)) # length values 
    logits <- matrix(NA, nrow=big.len, ncol=length(my.levels))
    logits[,1] <- rep(0,nrow(newdata)) # reference age
    for (i in 1:(n.levels-1)){
      logits[,(i+1)] <- Parameters[i] + Parameters[(n.levels-1+i)]*newdata$LENGTH   
    }
    # new code: to handle logit returns of large numbers  
    for (i in 1:n.levels){
      logits[logits[,i] >= 500,i] <- 500 
    }  
    p.unscaled <- exp(logits)
    p.normalized <- p.unscaled / rowSums(p.unscaled)
    p <- matrix(0, nrow=big.len, ncol=(big.age+1)) # note starting at age 0 
    for (i in 1:n.levels){
      p[,(my.levels[i]+1)] <- p.normalized[,i]
    }
    colnames(p)<-c(paste("pred.",0:big.age, sep=""))
    return(p)              
  }
  
  
  
  get.mult.props<-function(big.len=NULL,big.age=NULL,ref.age=NULL){
          data<-WtLenEx[!is.na(WtLenEx$AGE) & WtLenEx$AGE%in%big.age,]
          #data$AGE <- relevel(as.factor(data$AGE), ref=ref.age) # relevel and make categorical
          data$AGE <- as.factor(data$AGE)
          my.levels <- as.numeric(levels(data$AGE))
          n.levels <- length(my.levels)
          mn<-nnet::multinom(AGE ~ LENGTH, data=data,verbose=F)
          Parameters <- coefficients(mn) # Parameters for multinomial key
          newdata <- data.frame(cbind(LENGTH = big.len)) # length values
          logits <- matrix(NA, nrow=length(big.len), ncol=length(my.levels))
          logits[,1] <- rep(0,nrow(newdata)) # reference age
          for (i in 1:(n.levels-1)){
            logits[,(i+1)] <- Parameters[i] + Parameters[(n.levels-1+i)]*newdata$LENGTH
          }
          # new code: to handle logit returns of large numbers
          for (i in 1:n.levels){
            logits[logits[,i] >= 500,i] <- 500
          }
          p.unscaled <- exp(logits)
          p.normalized <- p.unscaled / rowSums(p.unscaled)
          p <- matrix(0, nrow=length(big.len), ncol=length(big.age)) # note starting at min big.age
          for (i in 1:n.levels){
            #p[,(my.levels[i]+1)] <- p.normalized[,i]
            p[,which(my.levels[i]==big.age)] <- p.normalized[,i]
          }
          colnames(p)<-c(paste("pred.",big.age, sep=""))
          return(p)
        } #end get.mult.props function