####################################################################################
##### DADOS COVID-19 PMCG ##########################################################
#################################################################################### 

             rm(list=ls(all=TRUE))

       set.seed(19)

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
           
##### Gráfico das notificações por dia
             
            par(mai=c(1.0,1.0,0.3,0.2))
           plot(dia, Q.CG, type="l", ylab="Quantidade", xlab="Dia")
         points(dia, Q.CG, pch=19)
             
##### Gráfico do total de notificações
             
            par(mai=c(1.0,1.0,0.3,0.2))
           plot(dia, QA.CG, type="l", ylab="Quantidade", xlab="Dia")
         points(dia, QA.CG, pch=19)
             
##### Distribuição de frequencias
             
          freq <- as.numeric(ftable(Q.CG))
      freq.rel <- round((freq/sum(freq))*100, 2)
      freq.rel
      
        barplot(freq.rel, names.arg = c(0,1,2,3,4,5,6,8,10,12,14), xlab="Quantidade de casos confirmados", ylab="Percentual", ylim=c(0,30))
           text(0.8, 22, "21,15%", cex=0.8)
           text(1.9, 16.2, "15,38%", cex=0.8)
           text(3.1, 25.7, "25%", cex=0.8)
           text(4.4, 16.2, "15,38%", cex=0.8)
           text(5.5, 6.5, "5,77%", cex=0.8)
           text(6.8, 8.5, "7,69%", cex=0.8)
           text(8.0, 2.6, "1,92%", cex=0.8)
           text(9.2, 2.6, "1,92%", cex=0.8)
           text(10.4, 2.6, "1,92%", cex=0.8)
           text(11.6, 2.6, "1,92%", cex=0.8)
           text(12.8, 2.6, "1,92%", cex=0.8)

        summary(Q.CG)
           sqrt(var(Q.CG))

##########################################################################
##### Ajuste dos Modelos #################################################
##########################################################################

########################   
##### Modelo Exponencial
########################

     model.exp <- nlsfit(D, model = 6, start = c(a = 10, b = 0.1))
     model.exp

#### Gráfico do modelo

        nlsplot(D, model = 6, start = c(a = 10, b = 0.1), xlab = "Days" , ylab = "Quantity", position = 1)

         alpha <- c(model.exp$Parameters[1,], model.exp$Parameters[2,])  #extracting coefficients

######################   
##### Modelo Logistico
######################
   
    model.logi <- nlsfit(D, model = 7, start = c(a = 100, b = 10, c = 1))
    model.logi

#### Gráfico do modelo

        nlsplot(D, model = 7, start = c(a = 100, b = 10, c = 1), xlab = "Days" , ylab = "Quantity", position = 1)

         alpha <- c(model.logi$Parameters[1,], model.logi$Parameters[2,], model.logi$Parameters[3,])  #extracting coefficients
     
#### Modelo estimado
      
model.logi.est <- function(d){
                alpha[1]/(1 + alpha[2]*exp(-alpha[3]*d))
               }
 
            d1 <- n
            ep <- 0

           dif <- 10
while(dif > ep){
            d1 <- d1 + 1
           dif <- round(alpha[1]) - round(model.logi.est(d1))
               }      
      
       EQM.log <- round(mean((D[,2] - model.logi.est(D[,1]))^2), 4)
           
##### Modelo Gompertz

    model.gomp <- nlsfit(D, model = 10, start = c(a = 1000, b = 5, c = 0.01))
    model.gomp
     
#### Gráfico do modelo
     
        nlsplot(D, model = 10, start = c(a = 1000, b = 5, c = 0.01), xlab = "Days" , ylab = "Quantity", position = 1)
     
         alpha <- c(model.gomp$Parameters[1,], model.gomp$Parameters[2,], model.gomp$Parameters[3,])  #extracting coefficients
     
#### Modelo estimado
     
model.gomp.est <- function(d){
              alpha[1]*exp(-alpha[2]*exp(-alpha[3]*d))
               }
     
            d1 <- n
            ep <- 0
     
           dif <- 10
while(dif > ep){
            d1 <- d1 + 1
           dif <- round(alpha[1]) - round(model.gomp.est(d1))
               }
     
            T1 <- log(alpha[2])/alpha[3]

###############################################            
##### Gráfico do modelo com o ponto de inflexão
###############################################            
            
            d0 <- seq(0,300)            
           plot(D[,1], D[,2], pch=19, lwd=1, xlim=c(0,300), ylim=c(0,610), ylab="Quantidade", xlab="Dia")
         points(d0, model.gomp.est(d0), type="l", lwd=2)
         points(T1, model.gomp.est(T1), pch=4, lwd=3, col="red")

            y1 <- seq(0, model.gomp.est(T1), 1)
            x1 <- rep(T1, length(y1))
         points(x1, y1, type="l", lty=2)

            x1 <- seq(-55, T1, 1)
            y1 <- rep(model.gomp.est(T1), length(x1))
         points(x1, y1, type="l", lty=2)

            x1 <- seq(-55, 300, 1)
            y1 <- rep(alpha[1], length(x1))
         points(x1, y1, type="l", lty=2)
           text(85, 220, "220")            
           text(115, 1, "Dia 66 (18/05/20)")            
           text(290, 613, "597")

########################################################            
##### Gráfico do uso dos leitos clínicos
########################################################            
 
            kt <- 20   # Clínico
        #    kt <- 14   # uti
            
            pl <- c(15, 20, 25) # clínico
        #    pl <- c(3,5,10)     # uti
           
  for(m in 1:3){         
           uta <- round((pl[m]/100)*model.gomp.est(d0), 6)
           nta <- length(uta)
           uti <- uta[2:nta] - uta[1:nta-1]
            nt <- length(uti) - kt
      
         q.uti <- numeric()
           
 for(i in 1:nt){
      q.uti[i] <- sum(uti[i:(i+kt-1)])     
               }
           
            x0 <- seq(1,length(q.uti))
             if(m==1)
           plot(x0, q.uti, type="l", lwd=2, xlab="Dia", ylab="Quantidade", ylim=c(0,10))
             if(m > 1)
         points(x0, q.uti, type="l", lwd=2, col=m)
               }
            
legend("topright", legend=c("15%", "20%", "25%"), col=c(1,2,3),lty=1)

max(q.uti)


