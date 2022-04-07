#ws82444

# przygotowanie bibliotek i danych

rm(list=ls())
install.packages("lmtest")
install.packages("car")
install.packages("tseries")
install.packages("sandwich")
install.packages("stargazer")
install.packages("normtest")
install.packages("AER")
install.packages("restriktor")
install.packages("moments")



library(lmtest)
library(car)
library(tseries)
library(sandwich)
library(stargazer)
library(normtest)
library("AER")
library(restriktor)
library(moments)
# ustawienie working directory
getwd()
setwd("C:/Users/Uzytkownik/Desktop/raport")
#pobranie danych
DATA=read.csv("ws82444r.csv",sep=";")
DATA=na.omit(DATA)
N=nrow(DATA)
DATA
# Oszacowanie modelu, analiza i testy
model=lm(lifeexp~healthexp+alcohol+GDP+infantmort+log(teacher), data=DATA)
summary(model)
#KORELOGRAM
install.packages("corrplot")
library(corrplot)
DATA1<-DATA[,2:7]
corrplot(cor(DATA1),type=c('upper'),diag=F)

# dopasowanie 
plot(DATA$lifeexp,main="Dopasowanie modelu",ylab="Wartoœci teoretyczne",xlab="lifeexp",lwd=2,col="pink")

#---------------------------------------------------------------------------------------#
# test RESET
reset(model,type="fitted")

# wspó³liniowoœæ
vif(model)

# testowanie normalnoœci sk³adnika losowego
install.packages("normtest")
library("normtest")
DATA$ehat <- model$residuals
jarque.bera.test(DATA$ehat)
 hist(DATA$ehat, main = "Rozk³ad reszt", col = "pink", xlab="Reszty")

# wykresy reszt
DATA$ehat2=model$residuals^2
plot(DATA$healthexp, DATA$ehat2, col="deeppink3", xlab="healthexp", ylab="Reszty")
plot(DATA$alcohol, DATA$ehat2, col="deeppink3", xlab="alcohol", ylab="Reszty")
plot(DATA$GDP, DATA$ehat2, col="deeppink3", xlab="GDP", ylab="Reszty")
plot(DATA$infantmort, DATA$ehat2, col="deeppink3", xlab="infantmort", ylab="Reszty")
plot(log(DATA$teacher), DATA$ehat2, col="deeppink3", xlab="healthexp", ylab="Reszty")

# test Breuscha-Pagana
bptest(model)

# test Gold-Quandta
gqtest(model,order.by = DATA$healthexp) 
gqtest(model,order.by = DATA$alcohol) 
gqtest(model,order.by = DATA$GDP) 
gqtest(model,order.by = DATA$infantmort)
gqtest(model,order.by = log(DATA$teacher)) 

# odporne b³êdy standardowe
VCOVHC=vcovHC(model,type="HC3")
coeftest(model)
coeftest(model,vcov.=VCOVHC)

# wa¿ona MNK


DATA$ehat2=model$residuals^2
auxiliary.lm=lm(log(ehat2)~healthexp+alcohol+GDP+infantmort+log(teacher), data=DATA)
DATA$weights=1/sqrt(exp(auxiliary.lm$fitted.values))
wls=lm(lifeexp~healthexp+alcohol+GDP+infantmort+log(teacher),data=DATA,weights=DATA$weights)
summary(wls)


# porównanie modeli
stargazer(model,wls,type="text")

#Metoda Zmiennych Instrumentalnych
#CZY INSTRUMENT JEST MOCNY?TESTOWANIE MOCY INSTRUMENTU TESTEM LINIOWYCH RESTRYKCJI
modelMOC <- lm(healthexp ~ immun, data=DATA)
linearHypothesis(modelMOC,"immun = 0")
#METODA ZMIENNYCH INSTRUMENTALNYCH

modelMZI1 <- ivreg(lifeexp ~ healthexp+alcohol+GDP+infantmort+log(teacher)|alcohol+GDP+infantmort+log(teacher)+immun, data=DATA)


#PODSUMOWANIE MODELU,TEST HAUSMANA
summary(modelMZI1, diagnostics = TRUE)
stargazer(model, modelMZI1, type="text")
