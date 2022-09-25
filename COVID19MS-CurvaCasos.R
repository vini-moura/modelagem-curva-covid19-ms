####################################################################################
##### DADOS COVID-19 PMCG ##########################################################
#################################################################################### 

       ALPHA <- matrix(0, nrow=7, ncol=3)

   ALPHA[1,] <- c(329.4358, 3.8762, 0.0297)  # -20
   ALPHA[2,] <- c(367.7685, 3.9270, 0.0277)  # -10
   ALPHA[3,] <- c(417.4884, 3.9962, 0.0258)  # 0
   ALPHA[4,] <- c(596.9272, 4.2699, 0.0219)  # Atual
   ALPHA[5,] <- c(632.2236, 4.2706, 0.0210)  # +25
   ALPHA[6,] <- c(1241.1720, 4.8103, 0.0161)  # +50
   ALPHA[7,] <- c(4776.2266, 6.0256, 0.0109)  # +75

          fg <- function(d){alpha[1]*exp(-alpha[2]*exp(-alpha[3]*d))}
          x0 <- seq(0,400,1)

####################             
##### Curva Gompertz
###################       

for(i in 1:6){
       alpha <- ALPHA[i,]
          y0 <- fg(x0)
               
           if(i==1)
         plot(x0, y0, type="l", lwd=2, xlim=c(0,450), ylim=c(0,1245), xlab="Dia", ylab="Quantidade")
               
           if(i>1)
       points(x0, y0, type="l", lwd=2, col=i)
               
           if(i==7)
       points(x0, y0, type="l", lwd=2, col=8)
             }
       
       
       legend("topleft", legend=c("-20%", "-10%", "0%","atual", "+25%", "+50%", "+75%"), col=c(1,2,3,4,5,6,8), lty=1)
      
         text(430, 305, "329")
         text(430, 360, "367")
         text(430, 425, "417")
         text(430, 590, "597")
         text(430, 640, "632")
         text(430, 1240, "1.241")
         text(430, 4775, "4.776")
       
##### Para i=7
       
          x0 <- seq(0,550,1)
       
           i <- 1
       alpha <- ALPHA[i,]
          y0 <- fg(x0)
         plot(x0, y0, type="l", lwd=2, xlim=c(0,600), ylim=c(0,4780), xlab="Dia", ylab="Quantidade", col="blue")
         
           i <- 7
       alpha <- ALPHA[i,]
          y0 <- fg(x0)
       points(x0, y0, type="l", lwd=2, col=8)
         
       legend("topleft", legend=c("atual", "+75%"), col=c(4,8), lty=1)
       
         text(580, 380, "597")
         text(590, 4700, "4.776")
       
######################################       
##### Curva média notificações diárias
######################################
       
for(i in 1:7){
       alpha <- ALPHA[i,]
          y0 <- fg(x0)
          n0 <- length(x0)
          d1 <- y0[1:(n0-1)]
          d2 <- y0[2:n0]
           d <- d2 - d1 
       
           if(i==1)
         plot(x0[1:(n0-1)], d, type="l", lwd=2, ylim=c(0,20), xlab="Dia", ylab="Quantidade média")
       
           if(i>1)
       points(x0[1:(n0-1)], d, type="l", lwd=2, col=i)
        
           if(i==7)
       points(x0[1:(n0-1)], d, type="l", lwd=2, col=8)
             }
       
       
legend("topright", legend=c("-20%", "-10%", "0%","atual", "+25%", "+50%", "+75%"), col=c(1,2,3,4,5,6,8), lty=1)

########################################################            
##### Gráfico do uso dos leitos clínicos
########################################################            

         alpha <- ALPHA[3,]

model.gomp.est <- function(d){alpha[1]*exp(-alpha[2]*exp(-alpha[3]*d))
               }

         #   kt <- 20   # Clínico
        #    pl <- c(15, 20, 25) # clínico
            
            kt <- 21   # uti
            pl <- c(3,5,10) #uti
            d0 <- seq(0,500) 
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
           plot(x0, q.uti, type="l", lwd=2, xlab="Dia", ylab="Quantidade", ylim=c(0,40))
             if(m > 1) 
         points(x0, q.uti, type="l", lwd=2, col=m)
               }

         legend("topright", legend=c("15%", "20%", "25%"), col=c(1,2,3),lty=1)
