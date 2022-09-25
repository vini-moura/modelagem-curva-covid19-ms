####################################################################################
##### PMCG #########################################################################
#################################################################################### 

           rm(list=ls(all=TRUE))

     set.seed(17)

      library(compiler)
    enableJIT(3)
      library(readODS)

####################################################################################

       dados <- read_ods("/Users/erlandison/Documents/Erlandson/ZConsultoria/2020/08_MS/MS.ods", sheet = 1)

        Q.CG <- as.numeric(dados[,3])
          nt <- length(Q.CG)

          nu <- nt - 0
        Q.CG <- Q.CG[1:nu]
           n <- length(Q.CG)
         dia <- seq(1, n, 1) - 1

       QA.CG <- cumsum(Q.CG)

           D <- data.frame(dia, QA.CG)

##########################################################################
##### Comando nlsfit #####################################################
##########################################################################

      library(easynls)

########################   
##### Subnotificações 
########################

           L <- 1000
           M <- 4
          su <- c(10, 7, 5, 3)       
   alpha.est <- matrix(0, nrow=L, ncol=3)
       ALPHA <- matrix(0, nrow=M, ncol=3)
   alfa1.int <- matrix(0, nrow=M, ncol=2)
   alfa2.int <- matrix(0, nrow=M, ncol=2)
   alfa3.int <- matrix(0, nrow=M, ncol=2)
   
for(m in 1:M){  

for(l in 1:L){   
          DS <- D
          XR <- sort(round(runif(nu, 0, su[m])))
      # XR[1] <- min(XR)
       
       Q.CGS <- Q.CG + round(XR)#
      DS[,2] <- cumsum(Q.CGS) 
         dia <- DS[,1]
      QA.CGS <- DS[,2] 
      
#############################   
##### Modelo Modelo Gompertz
############################

   model.gomp <- nlsfit(DS, model = 10, start = c(a = 1000, b = 2, c = 0.1))
  #    nlsplot(DS, model = 10, start = c(a = 1000, b = 5, c = 0.01), xlab = "Days" , ylab = "Quantity", position = 1)
      
      alpha.est[l,] <- c(model.gomp$Parameters[1,], model.gomp$Parameters[2,], model.gomp$Parameters[3,])  #extracting coefficients
cat('\n', m, l) } # fim de l
  
   
   alpha.mean <- apply(alpha.est, 2, mean)
    ALPHA[m,] <- alpha.mean
alfa1.int[m,] <- c(quantile(alpha.est[,1],0.025), quantile(alpha.est[,1],0.975))
alfa2.int[m,] <- c(quantile(alpha.est[,2],0.025), quantile(alpha.est[,2],0.975))
alfa3.int[m,] <- c(quantile(alpha.est[,3],0.025), quantile(alpha.est[,3],0.975))

####################################
##### Grafico atula e subnitificados
####################################

           fg <- function(x){alfa1*exp(-alfa2*exp(-alfa3*x))}
            x <- seq(0,290)
   
            if(m==1)
              {
           par(mai=c(1.0,1.0,0.3,0.2))
          plot(dia, QA.CG, pch=19, ylab="Quantidade", xlab="Dia", xlim=c(0,320), ylim=c(0,1500))

##### Atual

        alfa1 <- 596.9272
        alfa2 <- 4.2699
        alfa3 <- 0.0219

        points(x, fg(x), type="l", lwd=2, col="black")
           y1 <- rep(alfa1, length(x))
        points(x, y1, type="l", lty=2, col="black")
          text(315, 595, "597")
        
##### Subnotificados
        
        alfa1 <- alpha.mean[1]
        alfa2 <- alpha.mean[2]
        alfa3 <- alpha.mean[3]
        
### máximo 10 vezes
        
        alfa1 <- alpha.mean[1]
        alfa2 <- alpha.mean[2]
        alfa3 <- alpha.mean[3]
        points(x, fg(x), type="l", lwd=2, col="red")
           y1 <- rep(alfa1, length(x))
        points(x, y1, type="l", lty=2, col="red")
          text(315, 1400, "1.407")
              }
            
              
### máximo 7 vezes
        
            if(m == 2)
              {
          
        alfa1 <- alpha.mean[1]
        alfa2 <- alpha.mean[2]
        alfa3 <- alpha.mean[3]
              
        points(x, fg(x), type="l", lwd=2, col="magenta")
           y1 <- rep(alfa1, length(x))
        points(x, y1, type="l", lty=2, col="magenta")
          text(315, 1180, "1.181")
              }
            
            if(m == 3)
              {
              
### máximo 5 vezes
          
        alfa1 <- alpha.mean[1]
        alfa2 <- alpha.mean[2]
        alfa3 <- alpha.mean[3]
              
        points(x, fg(x), type="l", lwd=2, col="blue")
           y1 <- rep(alfa1, length(x))
        points(x, y1, type="l", lty=2, col="blue")
          text(315, 1020, "1.021")
              }
            
### máximo 3 vezes
        
            if(m==4)
              { 
              
        alfa1 <- alpha.mean[1]
        alfa2 <- alpha.mean[2]
        alfa3 <- alpha.mean[3]
              
        points(x, fg(x), type="l", lwd=2, col="green")
           y1 <- rep(alfa1, length(x))
        points(x, y1, type="l", lty=2, col="green")
          text(315, 855, "856")
              }
            
          # cat('\n', m)
              }
   
        legend("bottomright", legend = c("Atual","k=3","k=5","k=7","k=10"), col=c("black","green","blue","magenta","red"), lty=1, horiz=FALSE)
        
        
        
        
       