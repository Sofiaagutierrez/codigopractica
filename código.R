#Rcapture package

install.packages("Rcapture")
install.packages("FSA")


library(Rcapture)
captura <- read.csv("~/Desktop/rcapture.csv", header = FALSE)
View(captura)

openp(captura, dfreq = FALSE, m = c("ep"), neg = TRUE)

periodhist(captura, dfreq=FALSE, vt(18), drop=TRUE)


#Evaluar normalidad para abundancia 

normalidad_ab <- rnorm(n = 17, mean = 85.52352941  , sd =  21.29112284)

plot(density(normalidad_ab))

shapiro.test(normalidad_ab)$p.value

#Anova para abundancia Rcapture

anova1 <-aov(Abundance ~  year  , data = abyear)
  
  
summary(anova1)

#Evaluar normalidad para tasa de crecimiento

normalidad_tc <- rnorm (n = 16, mean = 0.03068833919 , sd = 0.1390599991)

plot(density(normalidad_tc))

shapiro.test(normalidad_tc)$p.value

#Evaluar linealidad tasa de crecimiento 

#Evaluar




#Paquete FSA

library(FSA)
hold.dat = capHistSum(Jolly, cols2use = -1)

pop.est <-mrOpen(hold.dat, type = c ("Jolly"), conf.level = 0.95 , 
                 phi.full = FALSE)             


summary(pop.est)
confint(pop.est)  


