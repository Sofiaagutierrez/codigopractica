# Rcapture package

library(Rcapture)
captura <- read.csv("~/Desktop/rcapture.csv", header = FALSE)
View(captura)

openp(captura, dfreq = FALSE, m = c("ep"), neg = TRUE)

periodhist(captura, dfreq=FALSE, vt(18), drop=TRUE)


#Paquete FSA

library(FSA)
hold.dat = capHistSum(Jolly, cols2use = -1)

pop.est <-mrOpen(hold.dat, type = c ("Jolly"), conf.level = 0.95 , 
                 phi.full = FALSE)             


summary(pop.est)
confint(pop.est)  


