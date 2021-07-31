#BaSTA















#Rcapture package

install.packages("Rcapture")
install.packages("FSA")


library(Rcapture)
captura <- read.csv("~/Desktop/rcapture.csv", header = FALSE)
View(captura)

openp(captura, dfreq = FALSE, m = c("ep"), neg = TRUE)

periodhist(captura, dfreq=FALSE, vt(18), drop=TRUE)


#Evaluar normalidad para abundancia 


library("ggplot2")
library("nortest")

ggplot(abyear) + geom_histogram(aes(x =  Abundance ,
                y =..density..) , binwidth = 14, fill = "grey", color = "black")
shapiro.test(abyear$Abundance)

#Anova para abundancia Rcapture

anova1 <-aov(Abundance~as.factor(year), data = abyear)
             
          
summary(anova1)

TukeyHSD(aov(Abundance~as.factor(year), data = abyear))

ptukey(anova1)


#Evaluar normalidad para tasa de crecimiento


ggplot(anovaT) + geom_histogram(aes(x =  tc , y =..density..) , binwidth = 0.05, fill = "grey", color = "black")
shapiro.test(anovaT$tc)


#Anova para tc Rcapture

anova2 <-aov(deltaN ~  Año  , data = anovaTC)

summary(anova2)

#Regresion lineal 
#contruir modelo

library(Matrix)
library(glmnet)
regresion =   anovaT$tc ~ anovaT$y

modelo1<- lm(regresion)
summary(modelo1)

#Evaluar linealidad tasa de crecimiento 

plot(modelo1, 1)
cor.test(anovaT$tc, anovaT$y)

#Evaluar normalidad tasa de crecimiento 

plot(modelo1, 2)
shapiro.test(modelo1$residuals)

#Evaluar homocedasticidad tasa de crecimiento 

plot(modelo1, 3)
library(carData)
library(car)
ncvTest(modelo1)


#Regresion lineal 

library(tidyverse)
library(boot)
library(QuantPsyc)
library(ggplot2)

model = lm(tc~ y , data = anovaT,  na.action=na.exclude)
summary(model)

grafica1 = ggplot(anovaT, aes(y,tc))
grafica1+ geom_point() + geom_smooth(method = "lm" , colour = "Red" )


#Paquete FSA

library(FSA)
hold.dat = capHistSum(Jolly, cols2use = -1)

pop.est <-mrOpen(hold.dat, type = c ("Jolly"), conf.level = 0.95 , 
                 phi.full = FALSE)             


summary(pop.est)
confint(pop.est)  


