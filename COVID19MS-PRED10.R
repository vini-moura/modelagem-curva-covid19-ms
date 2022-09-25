####################################################################################
##### DADOS COVID-19 PMCG ##########################################################
#################################################################################### 

##### SIMULAÇÃO +25%, 50%, 75%, -10%, -20%

           rm(list=ls(all=TRUE))

     set.seed(12)

      library(compiler)
    enableJIT(3)
      library(readODS)
      library(easynls)
    
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
##### Ajuste do Modelo Gompertz ##########################################
##########################################################################

########################   
##### Modelo Exponencial
########################

   model.exp <- nlsfit(D[(nu-13):nu,], model = 6, start = c(a = 1, b = 0.1))
       alpha <- c(model.exp$Parameters[1,], model.exp$Parameters[2,])  #extracting coefficients
   
#### Gráfico do modelo

          a0 <- D[nu,2]      # Ultimo valor observado
           a <- alpha[2]     # Taxa do modelo exponencial
           a <- a + 0.75*a   # Aumento (ou redução) da taxa
           
 mod.exp.est <- function(d){a0*exp(a*d)}

######################   
##### DADOS NOVOS
######################

          xn <- seq(1,10)
          xm <- seq((nu+1),(nu+10))
  
          ve <- sort(round(mod.exp.est(xn) + rnorm(length(xn), 0, 1))) 
          
          D1 <- data.frame(xm, ve)
colnames(D1) <- c("dia","QA.CG")

          DA <- rbind(D, D1)

         plot(dia, QA.CG, pch=19, xlim=c(0,(nt+9)), ylim=c(0,max(ve)), ylab="Quantidade", xlab="Dias")
       points(xm, ve, col="red")

################################   
##### Grafico das projeções ####
################################

         plot(dia, QA.CG, pch=19, xlim=c(0,(nt+9)), ylim=c(0,max(ve)), ylab="Quantidade", xlab="Dias")
       
       points(xm, ve, col="red", pch=20, lwd=0.1)
       points(xm, ve, col="red", type="l", lwd=1)
       
       points(xm, ve, col="green", pch=20, lwd=0.1)
       points(xm, ve, col="green", type="l", lwd=1)
       
       points(xm, ve, col="magenta", pch=20, lwd=0.1)
       points(xm, ve, col="magenta", type="l", lwd=1)
       
       points(xm, ve, col="blue", pch=20, lwd=0.1)
       points(xm, ve, col="blue", type="l", lwd=1)
       
       legend("topleft", legend=c("0%","25%","50%","75%"), col=c("blue","magenta","green","red"), pch=19)
       
#############################   
##### Modelo Modelo Gompertz
############################

  model.gomp <- nlsfit(DA, model = 10, start = c(a = 500, b = 50, c = 0.1))
  model.gomp
  
      alpha <- c(model.gomp$Parameters[1,], model.gomp$Parameters[2,], model.gomp$Parameters[3,])  #extracting coefficients
   
     nlsplot(DA, model = 10, start = c(a = 1000, b = 5, c = 0.01), xlab = "Days" , ylab = "Quantity", position = 1)
      
##### 75% 
    
#      model.gomp <- nlsfit(DA, model = 10, start = c(a = 1000, b = 2, c = 0.1))
#      model.gomp
#      nlsplot(DA, model = 10, start = c(a = 1000, b = 2, c = 0.1), xlab = "Days" , ylab = "Quantity", position = 1)
      
  
  T1 <- log(alpha[2])/alpha[3]
  T1
    
round(alpha, 4)


