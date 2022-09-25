####################################################################################
##### DADOS COVID PMCG #############################################################
#################################################################################### 

             rm(list=ls(all=TRUE))

       set.seed(19)

        library(compiler)
      enableJIT(3)
        library(readODS)

####################################################################################

         dados <- read_ods("/home/vinicius/Documents/Ciencias Econômicas/projeto COVID-19/Tres lagoas/Dados Três Lagoas.ods", sheet = 1)

          Q.CG <- as.numeric(dados[,3])
          
            nt <- length(Q.CG)

            nu <- nt - 7
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
             #fig6a
##### Distribuição de frequencias
             
          freq <- as.numeric(ftable(Q.CG))
      freq.rel <- round((freq/sum(freq))*100, 2)
      freq
      freq.rel
             
        summary(Q.CG)
          sqrt(var(Q.CG))
             
##########################################################################
##### Comando nlsfit #####################################################
##########################################################################

        library(easynls)

########################   
##### Modelo Exponencial
########################

     model.exp <- nlsfit(D, model = 6, start = c(a = 10, b = 0.1))
     model.exp

#### Gráfico do modelo

        nlsplot(D, model = 6, start = c(a = 10, b = 0.1), xlab = "Days" , ylab = "Quantity", position = 1)

         alpha <- c(model.exp$Parameters[1,], model.exp$Parameters[2,])  #extracting coefficients
#fig6b
######################   
##### Modelo Logistico
######################
   
    model.logi <- nlsfit(D, model = 7, start = c(a = 10, b = 10, c = 1))
    model.logi

#### Gráfico do modelo

        nlsplot(D, model = 7, start = c(a = 10, b = 10, c = 1), xlab = "Days" , ylab = "Quantity", position = 1)

         alpha <- c(model.logi$Parameters[1,], model.logi$Parameters[2,], model.logi$Parameters[3,])  #extracting coefficients
     #fig6c
#### Modelo estimado
      
model.logi.est <- function(d){
                alpha[1]/(1 + alpha[2]*exp(-alpha[3]*d))
               }

##### Modelo Gompertz

    model.gomp <- nlsfit(D, model = 10, start = c(a = 20, b = 1, c = 0.1))
    model.gomp
     
#### Gráfico do modelo
     
        nlsplot(D, model = 10, start = c(a = 1000, b = 5, c = 0.01), xlab = "Days" , ylab = "Quantity", position = 1)
     
         alpha <- c(model.gomp$Parameters[1,], model.gomp$Parameters[2,], model.gomp$Parameters[3,])  #extracting coefficients
     #fig6d
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
     
           d1    
           
            T1 <- log(alpha[2])/alpha[3]  # PORNTO DE INFLEXÃO

