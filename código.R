# Rcapture package 





#Evaluar normalidad de datos Rcapture


#ANOVA con datos Rcapture

#Evaluar supuestos para hacer Regresión con datos Rcapture



#Paquete FSA

library(FSA)
hold.dat = capHistSum(Jolly, cols2use = -1)

pop.est <-mrOpen(hold.dat, type = c ("Jolly"), conf.level = 0.95 , 
                 phi.full = FALSE)             


summary(pop.est)
confint(pop.est)  


