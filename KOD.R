library(stats)
library(lubridate)
library(tseries)
library(zoo)
library(FinTS)
library(e1071)
library(rugarch)
library(fGarch)
library(tidyverse)
library(forecast)
library(car)
library(fBasics)
library(ggplot2)
library(xts)
library(GAS)


# Trzy indeksy gieldowe (dane dzienne obejmujace okres 01.01.2000r - 31.12.2015r.)

#1. Nasdaq 100 (^NDX) https://stooq.pl/q/d/?s=%5Endx&c=0&d1=20000101&d2=20151231&o=1111111&o_s=1&o_d=1&o_p=1&o_n=1&o_o=1&o_m=1&o_x=1
#2. Nikkei 225 (^NKX) https://stooq.pl/q/d/?s=%5Enkx&c=0&d1=20000101&d2=20151231&o=1111111&o_s=1&o_d=1&o_p=1&o_n=1&o_o=1&o_m=1&o_x=1
#3. DAX (^DAX) https://stooq.pl/q/d/?s=%5Edax&c=0&d1=20000101&d2=20151231&o=1111111&o_s=1&o_d=1&o_p=1&o_n=1&o_o=1&o_m=1&o_x=1


# WCZYTANIE DANYCH-----------------------------------------------------------------------------------------------------------------------------------
# NDX
ndx <- read.csv("C:/Users/katar/OneDrive/Pulpit/projekt1/ndx.csv", row.names=1)
#ndx <- read.csv("ndx.csv", row.names=1)
ndx$Data <- NA
Data <- c(rownames(ndx))
ndx$Data <- as.Date(Data, format = '%Y-%m-%d') 

# NKX
nkx <- read.csv("C:/Users/katar/OneDrive/Pulpit/projekt1/nkx.csv", row.names=1)
nkx$Data <- NA
Data <- c(rownames(nkx))
nkx$Data <- as.Date(Data, format = '%Y-%m-%d') 

# DAX
dax <- read.csv("C:/Users/katar/OneDrive/Pulpit/projekt1/dax.csv", row.names=1)
dax$Data <- NA
Data <- c(rownames(dax))
dax$Data <- as.Date(Data, format = '%Y-%m-%d') 




#PODSTAWOWE STATYSTYKI ZMIENNEJ ZAMKNIECIE-----------------------------------------------------------------------------------------------------------
# NDX
round(basicStats(ndx$Zamkniecie),3)

# NKX
round(basicStats(nkx$Zamkniecie),3)

# DAX
round(basicStats(dax$Zamkniecie),3)



# PREZENTACJA DANYCH---------------------------------------------------------------------------------------------------------------------------------
# KURS ZAMKNIECIA 2000-2021 - NDX
ggplot(data=ndx,aes(x=Data,y=Zamkniecie))+geom_point(colour="black")+
  labs(x = "Data",y = "Kurs Zamkniecia")


ndx_stopy_1d_tab <- as.data.frame(ndx$Stopy_zwrotu_1d)
colnames(ndx_stopy_1d_tab) <- "Stopy_zwrotu_1d"
ndx_stopy_1d_tab$Data <- NA
Data <- c(rownames(ndx))
rownames(ndx_stopy_1d_tab) <- rownames(ndx)
ndx_stopy_1d_tab$Data <- as.Date(Data, format = '%Y-%m-%d') 
ndx_stopy_1d_tab <- ndx_stopy_1d_tab[-1,]

ndx_stopy_5d_tab <- as.data.frame(ndx$Stopy_zwrotu_5d)
colnames(ndx_stopy_5d_tab) <- "Stopy_zwrotu_5d"
ndx_stopy_5d_tab$Data <- NA
Data <- c(rownames(ndx))
rownames(ndx_stopy_5d_tab) <- rownames(ndx)
ndx_stopy_5d_tab$Data <- as.Date(Data, format = '%Y-%m-%d') 
ndx_stopy_5d_tab <- as.data.frame(na.omit(ndx_stopy_5d_tab))


# KURS ZAMKNIECIA 2000-2021 - NKX
ggplot(data=nkx,aes(x=Data,y=Zamkniecie))+geom_point(colour="black")+
  labs(x = "Data",y = "Kurs Zamkniecia")


nkx_stopy_1d_tab <- as.data.frame(nkx$Stopy_zwrotu_1d)
colnames(nkx_stopy_1d_tab) <- "Stopy_zwrotu_1d"
nkx_stopy_1d_tab$Data <- NA
Data <- c(rownames(nkx))
rownames(nkx_stopy_1d_tab) <- rownames(nkx)
nkx_stopy_1d_tab$Data <- as.Date(Data, format = '%Y-%m-%d') 
nkx_stopy_1d_tab <- nkx_stopy_1d_tab[-1,]

nkx_stopy_5d_tab <- as.data.frame(nkx$Stopy_zwrotu_5d)
colnames(nkx_stopy_5d_tab) <- "Stopy_zwrotu_5d"
nkx_stopy_5d_tab$Data <- NA
Data <- c(rownames(nkx))
rownames(nkx_stopy_5d_tab) <- rownames(nkx)
nkx_stopy_5d_tab$Data <- as.Date(Data, format = '%Y-%m-%d') 
nkx_stopy_5d_tab <- as.data.frame(na.omit(nkx_stopy_5d_tab))



# KURS ZAMKNIECIA 2000-2021 - DAX
ggplot(data=dax,aes(x=Data,y=Zamkniecie))+geom_point(colour="black")+
  labs(x = "Data",y = "Kurs Zamkniecia")


dax_stopy_1d_tab <- as.data.frame(dax$Stopy_zwrotu_1d)
colnames(dax_stopy_1d_tab) <- "Stopy_zwrotu_1d"
dax_stopy_1d_tab$Data <- NA
Data <- c(rownames(dax))
rownames(dax_stopy_1d_tab) <- rownames(dax)
dax_stopy_1d_tab$Data <- as.Date(Data, format = '%Y-%m-%d') 
dax_stopy_1d_tab <- dax_stopy_1d_tab[-1,]

dax_stopy_5d_tab <- as.data.frame(dax$Stopy_zwrotu_5d)
colnames(dax_stopy_5d_tab) <- "Stopy_zwrotu_5d"
dax_stopy_5d_tab$Data <- NA
Data <- c(rownames(dax))
rownames(dax_stopy_5d_tab) <- rownames(dax)
dax_stopy_5d_tab$Data <- as.Date(Data, format = '%Y-%m-%d') 
dax_stopy_5d_tab <- as.data.frame(na.omit(dax_stopy_5d_tab))





# WYKRESY I STATYSTYKI DZIENNYCH STOP ZWROTU------------------------------------------------------------------------------------------------------
# NDX
ggplot(data=ndx_stopy_1d_tab,aes(x=Data,y=Stopy_zwrotu_1d))+geom_line(colour="black")+
  labs(x = "Data",y = "Dzienne stopy zwrotu")
round(basicStats(ndx_stopy_1d_tab$Stopy_zwrotu_1d),3)


# NKX
ggplot(data=nkx_stopy_1d_tab,aes(x=Data,y=Stopy_zwrotu_1d))+geom_line(colour="black")+
  labs(x = "Data",y = "Dzienne stopy zwrotu")
round(basicStats(nkx_stopy_1d_tab$Stopy_zwrotu_1d),3)


# DAX
ggplot(data=dax_stopy_1d_tab,aes(x=Data,y=Stopy_zwrotu_1d))+geom_line(colour="black")+
  labs(x = "Data",y = "Dzienne stopy zwrotu")
round(basicStats(dax_stopy_1d_tab$Stopy_zwrotu_1d),3)



# BADANIE NA OBECNOSC AUTOKORELACJI DLA DZIENNYCH STOP ZWROTU (LICZBA OPOZNIEN = 10)-----------------------------------------------------------------

# KORELOGRAM DZIENNYCH STOP ZWROTU -------------------
# NDX
Acf(ndx_stopy_1d_tab$Stopy_zwrotu_1d,lag.max = 10, lwd =2,xlab="Opoznienie")


# NKX
Acf(nkx_stopy_1d_tab$Stopy_zwrotu_1d,lag.max = 10, lwd =2,xlab="Opoznienie")


# DAX
Acf(dax_stopy_1d_tab$Stopy_zwrotu_1d,lag.max = 10, lwd =2,xlab="Opoznienie")





# TEST LJUNGA - BOXA-----------------------------------

# NDX
ndx_stopy_1d_tab$autokorelacja10rzedow_p_value <-NA
for (i in c(1:10)) {
  p_value <-Box.test(ndx_stopy_1d_tab$Stopy_zwrotu_1d, type = 'Ljung-Box',lag = i)
  
  ndx_stopy_1d_tab$autokorelacja10rzedow_p_value[i] <- round(p_value$p.value,3)
}


# NKX
nkx_stopy_1d_tab$autokorelacja10rzedow_p_value <-NA
for (i in c(1:10)) {
  p_value <-Box.test(nkx_stopy_1d_tab$Stopy_zwrotu_1d, type = 'Ljung-Box',lag = i)
  
  nkx_stopy_1d_tab$autokorelacja10rzedow_p_value[i] <- round(p_value$p.value,3)
}


# DAX
dax_stopy_1d_tab$autokorelacja10rzedow_p_value <-NA
for (i in c(1:10)) {
  p_value <-Box.test(dax_stopy_1d_tab$Stopy_zwrotu_1d, type = 'Ljung-Box',lag = i)
  
  dax_stopy_1d_tab$autokorelacja10rzedow_p_value[i] <- round(p_value$p.value,3)
}





# BADANIE NA OBECNOSC STACJONARNOSCI DLA DZIENNYCH STOP ZWROTU (LICZBA OPOZNIEN = 10)-----------------------------------------------------------------
# NDX
adf.test(ndx_stopy_1d_tab$Stopy_zwrotu_1d, alternative = c("stationary"))

# NKX
adf.test(nkx_stopy_1d_tab$Stopy_zwrotu_1d, alternative = c("stationary"))

# DAX
adf.test(dax_stopy_1d_tab$Stopy_zwrotu_1d, alternative = c("stationary"))







###################################### TYGODNIOWE STOPY ####################


# WYKRESY I STATYSTYKI TYGODNIOWYCH (5 DNIOWYCH) STOP ZWROTU------------------------------------------------------------------------------------------------------
# NDX
ggplot(data=ndx_stopy_5d_tab,aes(x=Data,y=Stopy_zwrotu_5d))+geom_line(colour="black")+
  labs(x = "Data",y = "Tygodniowe stopy zwrotu")
round(basicStats(ndx_stopy_5d_tab$Stopy_zwrotu_5d),3)


# NKX
ggplot(data=nkx_stopy_5d_tab,aes(x=Data,y=Stopy_zwrotu_5d))+geom_line(colour="black")+
  labs(x = "Data",y = "Tygodniowe stopy zwrotu")
round(basicStats(nkx_stopy_5d_tab$Stopy_zwrotu_5d),3)


# DAX
ggplot(data=dax_stopy_5d_tab,aes(x=Data,y=Stopy_zwrotu_5d))+geom_line(colour="black")+
  labs(x = "Data",y = "Tygodniowe stopy zwrotu")
round(basicStats(dax_stopy_5d_tab$Stopy_zwrotu_5d),3)




# BADANIE NA OBECNOSC AUTOKORELACJI DLA TYGODNIOWYCH STOP ZWROTU (LICZBA OPOZNIEN = 10)-----------------------------------------------------------------

# KORELOGRAM TYGODNIOWYCH STOP ZWROTU -------------------
# NDX
Acf(ndx_stopy_5d_tab$Stopy_zwrotu_5d,lag.max = 10, lwd =2,xlab="Opoznienie")


# NKX
Acf(nkx_stopy_5d_tab$Stopy_zwrotu_5d,lag.max = 10, lwd =2,xlab="Opoznienie")


# DAX
Acf(dax_stopy_5d_tab$Stopy_zwrotu_5d,lag.max = 10, lwd =2,xlab="Opoznienie")





# TEST LJUNGA - BOXA-----------------------------------


# NDX
ndx_stopy_5d_tab$autokorelacja10rzedow_p_value <-NA
for (i in c(1:10)) {
  p_value <-Box.test(ndx_stopy_5d_tab$Stopy_zwrotu_5d, type = 'Ljung-Box',lag = i)
  
  ndx_stopy_5d_tab$autokorelacja10rzedow_p_value[i] <- round(p_value$p.value,3)
}


# NKX
nkx_stopy_5d_tab$autokorelacja10rzedow_p_value <-NA
for (i in c(1:10)) {
  p_value <-Box.test(nkx_stopy_5d_tab$Stopy_zwrotu_5d, type = 'Ljung-Box',lag = i)
  
  nkx_stopy_5d_tab$autokorelacja10rzedow_p_value[i] <- round(p_value$p.value,3)
}


# DAX
dax_stopy_5d_tab$autokorelacja10rzedow_p_value <-NA
for (i in c(1:10)) {
  p_value <-Box.test(dax_stopy_5d_tab$Stopy_zwrotu_5d, type = 'Ljung-Box',lag = i)
  
  dax_stopy_5d_tab$autokorelacja10rzedow_p_value[i] <- round(p_value$p.value,3)
}








# TWORZENIE SZEREGU CZASOWEGO------------------------------------------------------------------------------------------------------------------------ 
# NDX
ndx_ts_5d <- xts(ndx_stopy_5d_tab$Stopy_zwrotu_5d,ndx_stopy_5d_tab$Data)
is.xts(ndx_ts_5d)
colnames(ndx_ts_5d) <- c("Stopy_zwrotu_5d")

# NKX
nkx_ts_5d <- xts(nkx_stopy_5d_tab$Stopy_zwrotu_5d,nkx_stopy_5d_tab$Data)
is.xts(nkx_ts_5d)
colnames(nkx_ts_5d) <- c("Stopy_zwrotu_5d")

# DAX
dax_ts_5d <- xts(dax_stopy_5d_tab$Stopy_zwrotu_5d,dax_stopy_5d_tab$Data)
is.xts(dax_ts_5d)
colnames(dax_ts_5d) <- c("Stopy_zwrotu_5d")







#############################################################
#### CZESC 1 BADAN
### TYGODNIOWE STOPY OTRZYMANE PRZY POMOCY AR(1) ORAZ GARCH(1,1)
# HORYZONT CZASOWY = 100 TYGODNIOWYCH STOP ZWROTU
#########################################################################



# NDX---------------------------------------------------------------------------
# W Modelu GARCH(1,1) przyjeto warunkowy rozklad normalny

tt = 100
iloscWierszy_ndx <- nrow(ndx_ts_5d)
max_ndx = iloscWierszy_ndx-tt
#VaR 5%
ndx_ts_5d$VaR5_norm <- NA


for (i in c(1:max_ndx)){
  fragmentDanych_ndx <- ndx_ts_5d[i:(tt+i-1),]
  AR100_ndx = auto.arima(fragmentDanych_ndx$Stopy_zwrotu_5d)
  
  #prognoza  
  prognoza_AR100_ndx <- forecast(AR100_ndx)$mean[1]# prognoza nastepnej wartosci
  #reszty
  reszty <- AR100_ndx$residuals
  
  
  #Model GARCH(1,1) warunkowy rozklad normalny
  specyfikacja=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                          distribution.model = "norm")
  modelGarch_5d=ugarchfit(specyfikacja,reszty,solver ='hybrid')
  odchylenie=ugarchforecast(modelGarch_5d)@forecast$sigmaFor[1]  #warunkowe odch. stand. dla tt+1
  
  
  #VaR 5%
  kwantyl<-qnorm(0.05,mean=prognoza_AR100_ndx,sd=odchylenie)
  ndx_ts_5d$VaR5_norm[i+tt] <- kwantyl
}


# NDX
# W Modelu GARCH(1,1) przyjeto warunkowy rozklad t-Studenta

tt = 100
iloscWierszy_ndx <- nrow(ndx_ts_5d)
max_ndx = iloscWierszy_ndx-tt
#VaR 5%
ndx_ts_5d$VaR5_student <- NA


for (i in c(1:max_ndx)){
  fragmentDanych_ndx <- ndx_ts_5d[i:(tt+i-1),]
  AR100_ndx = auto.arima(fragmentDanych_ndx$Stopy_zwrotu_5d)
  
  #prognoza  
  prognoza_AR100_ndx <- forecast(AR100_ndx)$mean[1]# prognoza nastepnej wartosci
  #reszty
  reszty <- AR100_ndx$residuals
  
  
  #Model GARCH(1,1) warunkowy rozklad t-Studenta
  specyfikacja=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                          distribution.model = "std")
  modelGarch_5d=ugarchfit(specyfikacja,reszty,solver ='hybrid')
  odchylenie=ugarchforecast(modelGarch_5d)@forecast$sigmaFor[1]  #warunkowe odch. stand. dla tt+1
  
  
  #VaR 5%
  kwantyl<-qnorm(0.05,mean=prognoza_AR100_ndx,sd=odchylenie)
  ndx_ts_5d$VaR5_student[i+tt] <- kwantyl
}


# NKX---------------------------------------------------------------------------
# W Modelu GARCH(1,1) przyjeto warunkowy rozklad normalny

tt = 100
iloscWierszy_nkx <- nrow(nkx_ts_5d)
max_nkx = iloscWierszy_nkx-tt
#VaR 5%
nkx_ts_5d$VaR5_norm <- NA


for (i in c(1:max_nkx)){
  fragmentDanych_nkx <- nkx_ts_5d[i:(tt+i-1),]
  AR100_nkx = auto.arima(fragmentDanych_nkx$Stopy_zwrotu_5d)
  
  #prognoza  
  prognoza_AR100_nkx <- forecast(AR100_nkx)$mean[1]# prognoza nastepnej wartosci
  #reszty
  reszty <- AR100_nkx$residuals
  
  
  #Model GARCH(1,1) warunkowy rozklad normalny
  specyfikacja=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                          distribution.model = "norm")
  modelGarch_5d=ugarchfit(specyfikacja,reszty,solver ='hybrid')
  odchylenie=ugarchforecast(modelGarch_5d)@forecast$sigmaFor[1]  #warunkowe odch. stand. dla tt+1
  
  
  #VaR 5%
  kwantyl<-qnorm(0.05,mean=prognoza_AR100_nkx,sd=odchylenie)
  nkx_ts_5d$VaR5_norm[i+tt] <- kwantyl
}


# NKX
# W Modelu GARCH(1,1) przyjeto warunkowy rozklad t-Studenta

tt = 100
iloscWierszy_nkx <- nrow(nkx_ts_5d)
max_nkx = iloscWierszy_nkx-tt
#VaR 5%
nkx_ts_5d$VaR5_student <- NA


for (i in c(1:max_nkx)){
  fragmentDanych_nkx <- nkx_ts_5d[i:(tt+i-1),]
  AR100_nkx = auto.arima(fragmentDanych_nkx$Stopy_zwrotu_5d)
  
  #prognoza  
  prognoza_AR100_nkx <- forecast(AR100_nkx)$mean[1]# prognoza nastepnej wartosci
  #reszty
  reszty <- AR100_nkx$residuals
  
  
  #Model GARCH(1,1) warunkowy rozklad t-Studenta
  specyfikacja=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                          distribution.model = "std")
  modelGarch_5d=ugarchfit(specyfikacja,reszty,solver ='hybrid')
  odchylenie=ugarchforecast(modelGarch_5d)@forecast$sigmaFor[1]  #warunkowe odch. stand. dla tt+1
  
  
  #VaR 5%
  kwantyl<-qnorm(0.05,mean=prognoza_AR100_nkx,sd=odchylenie)
  nkx_ts_5d$VaR5_student[i+tt] <- kwantyl
}




# DAX---------------------------------------------------------------------------
# W Modelu GARCH(1,1) przyjeto warunkowy rozklad normalny
tt = 100
iloscWierszy_dax <- nrow(dax_ts_5d)
max_dax = iloscWierszy_dax-tt
#VaR 5%
dax_ts_5d$VaR5_norm <- NA


for (i in c(1:max_dax)){
  fragmentDanych_dax <- dax_ts_5d[i:(tt+i-1),]
  AR100_dax = auto.arima(fragmentDanych_dax$Stopy_zwrotu_5d)
  
  #prognoza  
  prognoza_AR100_dax <- forecast(AR100_dax)$mean[1]# prognoza nastepnej wartosci
  #reszty
  reszty <- AR100_dax$residuals
  
  
  #Model GARCH(1,1) warunkowy rozklad normalny
  specyfikacja=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                          distribution.model = "norm")
  modelGarch_5d=ugarchfit(specyfikacja,reszty,solver ='hybrid')
  odchylenie=ugarchforecast(modelGarch_5d)@forecast$sigmaFor[1]  #warunkowe odch. stand. dla tt+1
  
  
  #VaR 5%
  kwantyl<-qnorm(0.05,mean=prognoza_AR100_dax,sd=odchylenie)
  dax_ts_5d$VaR5_norm[i+tt] <- kwantyl
}


# DAX
# W Modelu GARCH(1,1) przyjeto warunkowy rozklad t-Studenta
tt = 100
iloscWierszy_dax <- nrow(dax_ts_5d)
max_dax = iloscWierszy_dax-tt
#VaR 5%
dax_ts_5d$VaR5_student <- NA


for (i in c(1:max_dax)){
  fragmentDanych_dax <- dax_ts_5d[i:(tt+i-1),]
  AR100_dax = auto.arima(fragmentDanych_dax$Stopy_zwrotu_5d)
  
  #prognoza  
  prognoza_AR100_dax <- forecast(AR100_dax)$mean[1]# prognoza nastepnej wartosci
  #reszty
  reszty <- AR100_dax$residuals
  
  
  #Model GARCH(1,1) warunkowy rozklad t-Studenta
  specyfikacja=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                          distribution.model = "std")
  modelGarch_5d=ugarchfit(specyfikacja,reszty,solver ='hybrid')
  odchylenie=ugarchforecast(modelGarch_5d)@forecast$sigmaFor[1]  #warunkowe odch. stand. dla tt+1
  
  
  #VaR 5%
  kwantyl<-qnorm(0.05,mean=prognoza_AR100_dax,sd=odchylenie)
  dax_ts_5d$VaR5_student[i+tt] <- kwantyl
}






#############################################################
#### CZESC 2 BADAN
### TYGODNIOWE STOPY OTRZYMANE PRZY POMOCY METODY MONTE CARLO
# HORYZONT CZASOWY = 500 DZIENNYCH STOP ZWROTU
#########################################################################





# METODA MONTE CARLO---------------------------------------------------------------------------------------------------------------------------------

# NDX
ndx_ts_1d <- xts(ndx_stopy_1d_tab$Stopy_zwrotu_1d,ndx_stopy_1d_tab$Data)
is.xts(ndx_ts_1d)
colnames(ndx_ts_1d) <- c("Stopy_zwrotu_1d")


# NKX
nkx_ts_1d <- xts(nkx_stopy_1d_tab$Stopy_zwrotu_1d,nkx_stopy_1d_tab$Data)
is.xts(nkx_ts_1d)
colnames(nkx_ts_1d) <- c("Stopy_zwrotu_1d")


# DAX
dax_ts_1d <- xts(dax_stopy_1d_tab$Stopy_zwrotu_1d,dax_stopy_1d_tab$Data)
is.xts(dax_ts_1d)
colnames(dax_ts_1d) <- c("Stopy_zwrotu_1d")



# METODA MONTE CARLO DLA 500 DZIENNYCH STOP ZWROTU----------------------------------------------------------------------------------------------------------

# NDX---------------------------------------------------------------------------
t = 500
iloscWierszy_ndx <- nrow(ndx_ts_1d)
max_ndx = iloscWierszy_ndx-t
#VaR 5%
ndx_ts_1d$VaR5_MC_norm <- NA
ndx_ts_1d$VaR5_MC_student <- NA

for (i in c(1:max_ndx)){
  
  if(i %% 5 == 0 || i ==1) { # czy i jest podzielne przez 5 - jesli tak reszta rowna sie zero
    
    fragmentDanych_ndx <- ndx_ts_1d[i:(t+i-1),]
    
    AR100_ndx = auto.arima(fragmentDanych_ndx$Stopy_zwrotu_1d)
    prognoza_AR100_ndx <- forecast(AR100_ndx)$mean[1:5]
    reszty <- AR100_ndx$residuals
    
    
    #Model GARCH(1,1) warunkowy rozklad normalny
    specyfikacja1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                             distribution.model = "norm")
    modelGarch_5d1=ugarchfit(specyfikacja1,reszty,solver ='hybrid')
    odchylenie1=ugarchforecast(modelGarch_5d1)@forecast$sigmaFor[1:5]  #warunkowe odch. stand. dla tt+1
    
    
    x1=rnorm(1000,mean=prognoza_AR100_ndx[1],sd=odchylenie1[1])
    x2=rnorm(1000,mean=prognoza_AR100_ndx[2],sd=odchylenie1[2])
    x3=rnorm(1000,mean=prognoza_AR100_ndx[3],sd=odchylenie1[3])
    x4=rnorm(1000,mean=prognoza_AR100_ndx[4],sd=odchylenie1[4])
    x5=rnorm(1000,mean=prognoza_AR100_ndx[5],sd=odchylenie1[5])
    x_norm=x1+x2+x3+x4+x5
    
    
    
    
    #Model GARCH(1,1) warunkowy rozklad t-Studenta 
    specyfikacja2=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                             distribution.model = "std")
    modelGarch_5d2=ugarchfit(specyfikacja2,reszty,solver ='hybrid')
    odchylenie2=ugarchforecast(modelGarch_5d2)@forecast$sigmaFor[1:5]  #warunkowe odch. stand. dla tt+1
    
    
    x1=rnorm(1000,mean=prognoza_AR100_ndx[1],sd=odchylenie2[1])
    x2=rnorm(1000,mean=prognoza_AR100_ndx[2],sd=odchylenie2[2])
    x3=rnorm(1000,mean=prognoza_AR100_ndx[3],sd=odchylenie2[3])
    x4=rnorm(1000,mean=prognoza_AR100_ndx[4],sd=odchylenie2[4])
    x5=rnorm(1000,mean=prognoza_AR100_ndx[5],sd=odchylenie2[5])
    x_std=x1+x2+x3+x4+x5
    
    
    #VaR 5%
    kwantyl_norm <-quantile(x_norm,0.05)
    kwantyl_std <-quantile(x_std,0.05)
    
    ndx_ts_1d$VaR5_MC_norm[i+t] <- kwantyl_norm # z tego wziac co piata wartosc i na tym robic
    ndx_ts_1d$VaR5_MC_student[i+t] <- kwantyl_std # z tego wziac co piata wartosc i na tym robic
    
    
    
    i=i+1
  } else{
    ndx_ts_1d$VaR5_MC_norm[i+t]=NA
    ndx_ts_1d$VaR5_MC_student[i+t]=NA
  }
}




# NKX---------------------------------------------------------------------------
t = 500
iloscWierszy_nkx <- nrow(nkx_ts_1d)
max_nkx = iloscWierszy_nkx-t
#VaR 5%
nkx_ts_1d$VaR5_MC_norm <- NA
nkx_ts_1d$VaR5_MC_student <- NA

for (i in c(1:max_nkx)){
  
  if(i %% 5 == 0 || i ==1) { # czy i jest podzielne przez 5 - jesli tak reszta rowna sie zero
    
    fragmentDanych_nkx <- nkx_ts_1d[i:(t+i-1),]
    
    AR100_nkx = auto.arima(fragmentDanych_nkx$Stopy_zwrotu_1d)
    prognoza_AR100_nkx <- forecast(AR100_nkx)$mean[1:5]
    reszty <- AR100_nkx$residuals
    
    
    #Model GARCH(1,1) warunkowy rozklad normalny
    specyfikacja1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                             distribution.model = "norm")
    modelGarch_5d1=ugarchfit(specyfikacja1,reszty,solver ='hybrid')
    odchylenie1=ugarchforecast(modelGarch_5d1)@forecast$sigmaFor[1:5]  #warunkowe odch. stand. dla tt+1
    
    
    x1=rnorm(1000,mean=prognoza_AR100_nkx[1],sd=odchylenie1[1])
    x2=rnorm(1000,mean=prognoza_AR100_nkx[2],sd=odchylenie1[2])
    x3=rnorm(1000,mean=prognoza_AR100_nkx[3],sd=odchylenie1[3])
    x4=rnorm(1000,mean=prognoza_AR100_nkx[4],sd=odchylenie1[4])
    x5=rnorm(1000,mean=prognoza_AR100_nkx[5],sd=odchylenie1[5])
    x_norm=x1+x2+x3+x4+x5
    
    
    
    
    #Model GARCH(1,1) warunkowy rozklad t-Studenta 
    specyfikacja2=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                             distribution.model = "std")
    modelGarch_5d2=ugarchfit(specyfikacja2,reszty,solver ='hybrid')
    odchylenie2=ugarchforecast(modelGarch_5d2)@forecast$sigmaFor[1:5]  #warunkowe odch. stand. dla tt+1
    
    
    x1=rnorm(1000,mean=prognoza_AR100_nkx[1],sd=odchylenie2[1])
    x2=rnorm(1000,mean=prognoza_AR100_nkx[2],sd=odchylenie2[2])
    x3=rnorm(1000,mean=prognoza_AR100_nkx[3],sd=odchylenie2[3])
    x4=rnorm(1000,mean=prognoza_AR100_nkx[4],sd=odchylenie2[4])
    x5=rnorm(1000,mean=prognoza_AR100_nkx[5],sd=odchylenie2[5])
    x_std=x1+x2+x3+x4+x5
    
    
    #VaR 5%
    kwantyl_norm <-quantile(x_norm,0.05)
    kwantyl_std <-quantile(x_std,0.05)
    
    nkx_ts_1d$VaR5_MC_norm[i+t] <- kwantyl_norm # z tego wziac co piata wartosc i na tym robic
    nkx_ts_1d$VaR5_MC_student[i+t] <- kwantyl_std # z tego wziac co piata wartosc i na tym robic
    
    
    
    i=i+1
  } else{
    nkx_ts_1d$VaR5_MC_norm[i+t]=NA
    nkx_ts_1d$VaR5_MC_student[i+t]=NA
  }
}







# DAX---------------------------------------------------------------------------
t = 500
iloscWierszy_dax <- nrow(dax_ts_1d)
max_dax = iloscWierszy_dax-t
#VaR 5%
dax_ts_1d$VaR5_MC_norm <- NA
dax_ts_1d$VaR5_MC_student <- NA

for (i in c(1:max_dax)){
  
  if(i %% 5 == 0 || i ==1) { # czy i jest podzielne przez 5 - jesli tak reszta rowna sie zero
    
    fragmentDanych_dax <- dax_ts_1d[i:(t+i-1),]
    
    AR100_dax = auto.arima(fragmentDanych_dax$Stopy_zwrotu_1d)
    prognoza_AR100_dax <- forecast(AR100_dax)$mean[1:5]
    reszty <- AR100_dax$residuals
    
    
    #Model GARCH(1,1) warunkowy rozklad normalny
    specyfikacja1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                             distribution.model = "norm")
    modelGarch_5d1=ugarchfit(specyfikacja1,reszty,solver ='hybrid')
    odchylenie1=ugarchforecast(modelGarch_5d1)@forecast$sigmaFor[1:5]  #warunkowe odch. stand. dla tt+1
    
    
    x1=rnorm(1000,mean=prognoza_AR100_dax[1],sd=odchylenie1[1])
    x2=rnorm(1000,mean=prognoza_AR100_dax[2],sd=odchylenie1[2])
    x3=rnorm(1000,mean=prognoza_AR100_dax[3],sd=odchylenie1[3])
    x4=rnorm(1000,mean=prognoza_AR100_dax[4],sd=odchylenie1[4])
    x5=rnorm(1000,mean=prognoza_AR100_dax[5],sd=odchylenie1[5])
    x_norm=x1+x2+x3+x4+x5
    
    
    
    
    #Model GARCH(1,1) warunkowy rozklad t-Studenta 
    specyfikacja2=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                             distribution.model = "std")
    modelGarch_5d2=ugarchfit(specyfikacja2,reszty,solver ='hybrid')
    odchylenie2=ugarchforecast(modelGarch_5d2)@forecast$sigmaFor[1:5]  #warunkowe odch. stand. dla tt+1
    
    
    x1=rnorm(1000,mean=prognoza_AR100_dax[1],sd=odchylenie2[1])
    x2=rnorm(1000,mean=prognoza_AR100_dax[2],sd=odchylenie2[2])
    x3=rnorm(1000,mean=prognoza_AR100_dax[3],sd=odchylenie2[3])
    x4=rnorm(1000,mean=prognoza_AR100_dax[4],sd=odchylenie2[4])
    x5=rnorm(1000,mean=prognoza_AR100_dax[5],sd=odchylenie2[5])
    x_std=x1+x2+x3+x4+x5
    
    
    #VaR 5%
    kwantyl_norm <-quantile(x_norm,0.05)
    kwantyl_std <-quantile(x_std,0.05)
    
    dax_ts_1d$VaR5_MC_norm[i+t] <- kwantyl_norm # z tego wziac co piata wartosc i na tym robic
    dax_ts_1d$VaR5_MC_student[i+t] <- kwantyl_std # z tego wziac co piata wartosc i na tym robic
    
    
    
    i=i+1
  } else{
    dax_ts_1d$VaR5_MC_norm[i+t]=NA
    dax_ts_1d$VaR5_MC_student[i+t]=NA
  }
}


#################################################################
###### TYGODNIOWE OTRZYMANE ZE SKUMULOWANYCH STOP DZIENNYCH ###########
########################################################################

# WYKRESY OTRZYMANEGO VaR 5% I ODPOWIADAJACYCH IM STOP ZWROTU------------------------------------------------------------------------------------
# KOLOR CZERWONY - Value at Risk, KOLOR CZARNY - Stopy zwrotu 

# VALUE AT RISK 5% DLA TYGODNIOWYCH ST.ZWROTU - WARUNKOWY ROZKLAD NORMALNY------

# NDX
plot(ndx_ts_5d$Stopy_zwrotu_5d[101:(dim(ndx_ts_5d)[1])], main = NULL)
lines(ndx_ts_5d$VaR5_norm[101:(dim(ndx_ts_5d)[1])], col = "red", lwd =2)
addLegend(legend.loc = "topright", legend.names = c("Tygodniowe stopy zwrotu","VaR 5% (rozklad normalny)"),
          col=c("black","red"), cex=1,
          title="Legenda", lwd = 4, text.font = 4)



# NKX
plot(nkx_ts_5d$Stopy_zwrotu_5d[101:(dim(nkx_ts_5d)[1])], main = NULL)
lines(nkx_ts_5d$VaR5_norm[101:(dim(nkx_ts_5d)[1])], col = "red", lwd =2)
addLegend(legend.loc = "topright", legend.names = c("Tygodniowe stopy zwrotu","VaR 5% (rozklad normalny)"),
          col=c("black","red"), cex=1,
          title="Legenda", lwd = 4, text.font = 4)

# DAX
plot(dax_ts_5d$Stopy_zwrotu_5d[101:(dim(dax_ts_5d)[1])], main = NULL)
lines(dax_ts_5d$VaR5_norm[101:(dim(dax_ts_5d)[1])], col = "red", lwd =2)
addLegend(legend.loc = "topright", legend.names = c("Tygodniowe stopy zwrotu","VaR 5% (rozklad normalny)"),
          col=c("black","red"), cex=1,
          title="Legenda", lwd = 4, text.font = 4)



# VALUE AT RISK 5% DLA TYGODNIOWYCH ST.ZWROTU - WARUNKOWY ROZKLAD T-STUDENTA----

# NDX40
plot(ndx_ts_5d$Stopy_zwrotu_5d[101:(dim(ndx_ts_5d)[1])], main = NULL)
lines(ndx_ts_5d$VaR5_student[101:(dim(ndx_ts_5d)[1])], col = "red", lwd =2)
addLegend(legend.loc = "topright", legend.names = c("Tygodniowe stopy zwrotu","VaR 5% (rozklad t-Studenta)"),
          col=c("black","red"), cex=1,
          title="Legenda", lwd = 4, text.font = 4)



# NKX
plot(nkx_ts_5d$Stopy_zwrotu_5d[101:(dim(nkx_ts_5d)[1])], main = NULL)
lines(nkx_ts_5d$VaR5_student[101:(dim(nkx_ts_5d)[1])], col = "red", lwd =2)
addLegend(legend.loc = "topright", legend.names = c("Tygodniowe stopy zwrotu","VaR 5% (rozklad t-Studenta)"),
          col=c("black","red"), cex=1,
          title="Legenda", lwd = 4, text.font = 4)


# DAX
plot(dax_ts_5d$Stopy_zwrotu_5d[101:(dim(dax_ts_5d)[1])], main = NULL)
lines(dax_ts_5d$VaR5_student[101:(dim(dax_ts_5d)[1])], col = "red", lwd =2)
addLegend(legend.loc = "topright", legend.names = c("Tygodniowe stopy zwrotu","VaR 5% (rozklad t-Studenta)"),
          col=c("black","red"), cex=1,
          title="Legenda", lwd = 4, text.font = 4)




VaR_ndx_MC <- cbind(na.omit(ndx_ts_1d$VaR5_MC_norm),na.omit(ndx_ts_1d$VaR5_MC_student))
VaR_nkx_MC <- cbind(na.omit(nkx_ts_1d$VaR5_MC_norm),na.omit(nkx_ts_1d$VaR5_MC_student))
VaR_dax_MC <- cbind(na.omit(dax_ts_1d$VaR5_MC_norm),na.omit(dax_ts_1d$VaR5_MC_student))


#############################################################################
### TYGODNIOWE Z MONTE CARLO
##################################################

# VALUE AT RISK 5% DLA DZIENNYCH ST.ZWROTU--------------------------------------

# NDX
plot(ndx_ts_5d$Stopy_zwrotu_5d[101:(dim(ndx_ts_5d)[1])], main = NULL)
## Warunkowy rozklad Normalny
lines(VaR_ndx_MC$VaR5_MC_norm, col = "red", lwd =2)
## Warunkowy rozklad t-Studenta
lines(VaR_ndx_MC$VaR5_MC_student, col = "blue", lwd =2, lty="dashed")
addLegend(legend.loc = "topright", legend.names = c("Tygodniowe stopy zwrotu","VaR 5% (rozklad normalny)","VaR 5% (rozklad t-Studenta)"),
          col=c("black","red", "blue"), cex=1,
          title="Legenda", lwd = 4, text.font = 4)



# NKX
plot(nkx_ts_5d$Stopy_zwrotu_5d[101:(dim(nkx_ts_5d)[1])], main = NULL)
## Warunkowy rozklad Normalny
lines(VaR_nkx_MC$VaR5_MC_norm, col = "red", lwd =2)
## Warunkowy rozklad t-Studenta
lines(VaR_nkx_MC$VaR5_MC_student, col = "blue", lwd =2, lty="dashed")
addLegend(legend.loc = "topright", legend.names = c("Tygodniowe stopy zwrotu","VaR 5% (rozklad normalny)","VaR 5% (rozklad t-Studenta)"),
          col=c("black","red", "blue"), cex=1,
          title="Legenda", lwd = 4, text.font = 4)



# DAX
plot(dax_ts_5d$Stopy_zwrotu_5d[101:(dim(dax_ts_5d)[1])], main = NULL)
## Warunkowy rozklad Normalny
lines(VaR_dax_MC$VaR5_MC_norm, col = "red", lwd =2)
## Warunkowy rozklad t-Studenta
lines(VaR_dax_MC$VaR5_MC_student, col = "blue", lwd =2, lty="dashed")
addLegend(legend.loc = "topright", legend.names = c("Tygodniowe stopy zwrotu","VaR 5% (rozklad normalny)","VaR 5% (rozklad t-Studenta)"),
          col=c("black","red", "blue"), cex=1,
          title="Legenda", lwd = 4, text.font = 4)





# TESTY WSTECZNE DLA TYGODNIOWYCH VaR 5%--------------------------------------------------------------------------------------------------------------

# TEST BEZWARUNKOWEGO POKRYCIA VAR - TEST KUPCA--------------------------------------------------------------------
# Hipoteza zerowa (H0) - Model VaR wyznaczony poprawnie
# Hipoteza alternatywna (H1) - Model VaR wyznaczony niepoprawnie

# NDX
# Warunkowy rozklad Normalny
ndx_t.Kupca_NORMALNY <- (BacktestVaR(ndx_ts_5d$Stopy_zwrotu_5d[101:(dim(ndx_ts_5d)[1])],na.omit(ndx_ts_5d$VaR5_norm), 0.05, Lags = 10))$LRuc
round(ndx_t.Kupca_NORMALNY,3)
# Warunkowy rozklad t-Studenta
ndx_t.Kupca_STUDENT <- (BacktestVaR(ndx_ts_5d$Stopy_zwrotu_5d[101:(dim(ndx_ts_5d)[1])],na.omit(ndx_ts_5d$VaR5_student), 0.05, Lags = 10))$LRuc
round(ndx_t.Kupca_STUDENT,3)
#######
#Monte Carlo - Warunkowy rozklad Normalny
ndx_t.Kupca_MC_NORMALNY <- (BacktestVaR(ndx_ts_5d$Stopy_zwrotu_5d[100:(dim(ndx_ts_5d)[1])],VaR_ndx_MC$VaR5_MC_norm, 0.05, Lags = 10))$LRuc
round(ndx_t.Kupca_MC_NORMALNY,3)
#Monte Carlo - Warunkowy rozklad t-Studenta
ndx_t.Kupca_MC_STUDENT <- (BacktestVaR(ndx_ts_5d$Stopy_zwrotu_5d[100:(dim(ndx_ts_5d)[1])],VaR_ndx_MC$VaR5_MC_student, 0.05, Lags = 10))$LRuc
round(ndx_t.Kupca_MC_STUDENT,3)





# NKX
# Warunkowy rozklad Normalny
nkx_t.Kupca_NORMALNY <- (BacktestVaR(nkx_ts_5d$Stopy_zwrotu_5d[101:(dim(nkx_ts_5d)[1])],na.omit(nkx_ts_5d$VaR5_norm), 0.05, Lags = 10))$LRuc
round(nkx_t.Kupca_NORMALNY,3)
# Warunkowy rozklad t-Studenta
nkx_t.Kupca_STUDENT <- (BacktestVaR(nkx_ts_5d$Stopy_zwrotu_5d[101:(dim(nkx_ts_5d)[1])],na.omit(nkx_ts_5d$VaR5_student), 0.05, Lags = 10))$LRuc
round(nkx_t.Kupca_STUDENT,3)
#######
#Monte Carlo - Warunkowy rozklad Normalny
nkx_t.Kupca_MC_NORMALNY <- (BacktestVaR(nkx_ts_5d$Stopy_zwrotu_5d[100:(dim(nkx_ts_5d)[1])],VaR_nkx_MC$VaR5_MC_norm, 0.05, Lags = 10))$LRuc
round(nkx_t.Kupca_MC_NORMALNY,3)
#Monte Carlo - Warunkowy rozklad t-Studenta
nkx_t.Kupca_MC_STUDENT <- (BacktestVaR(nkx_ts_5d$Stopy_zwrotu_5d[100:(dim(nkx_ts_5d)[1])],VaR_nkx_MC$VaR5_MC_student, 0.05, Lags = 10))$LRuc
round(nkx_t.Kupca_MC_STUDENT,3)




# DAX
# Warunkowy rozklad Normalny
dax_t.Kupca_NORMALNY <- (BacktestVaR(dax_ts_5d$Stopy_zwrotu_5d[101:(dim(dax_ts_5d)[1])],na.omit(dax_ts_5d$VaR5_norm), 0.05, Lags = 10))$LRuc
round(dax_t.Kupca_NORMALNY,3)
# Warunkowy rozklad t-Studenta
dax_t.Kupca_STUDENT <- (BacktestVaR(dax_ts_5d$Stopy_zwrotu_5d[101:(dim(dax_ts_5d)[1])],na.omit(dax_ts_5d$VaR5_student), 0.05, Lags = 10))$LRuc
round(dax_t.Kupca_STUDENT,3)
#######
#Monte Carlo - Warunkowy rozklad Normalny
dax_t.Kupca_MC_NORMALNY <- (BacktestVaR(dax_ts_5d$Stopy_zwrotu_5d[100:(dim(dax_ts_5d)[1])],VaR_dax_MC$VaR5_MC_norm, 0.05, Lags = 10))$LRuc
round(dax_t.Kupca_MC_NORMALNY,3)
#Monte Carlo - Warunkowy rozklad t-Studenta
dax_t.Kupca_MC_STUDENT <- (BacktestVaR(dax_ts_5d$Stopy_zwrotu_5d[100:(dim(dax_ts_5d)[1])],VaR_dax_MC$VaR5_MC_student, 0.05, Lags = 10))$LRuc
round(dax_t.Kupca_MC_STUDENT,3)






# TEST WARUNKOWEGO POKRYCIA VAR - TEST CHRISTOFFERSENA----------------------------------------------------------
# Hipoteza zerowa (H0) - Model VaR wyznaczony poprawnie
# Hipoteza alternatywna (H1) - Model VaR wyznaczony niepoprawnie

# NDX
# Warunkowy rozklad Normalny
ndx_t.Christoffersena_NORMALNY <- (BacktestVaR(ndx_ts_5d$Stopy_zwrotu_5d[101:(dim(ndx_ts_5d)[1])],na.omit(ndx_ts_5d$VaR5_norm), 0.05, Lags = 10))$LRuc
round(ndx_t.Christoffersena_NORMALNY,3)
# Warunkowy rozklad t-Studenta
ndx_t.Christoffersena_STUDENT <- (BacktestVaR(ndx_ts_5d$Stopy_zwrotu_5d[101:(dim(ndx_ts_5d)[1])],na.omit(ndx_ts_5d$VaR5_student), 0.05, Lags = 10))$LRuc
round(ndx_t.Christoffersena_STUDENT,3)
#######
#Monte Carlo - Warunkowy rozklad Normalny
ndx_t.Christoffersena_MC_NORMALNY <- (BacktestVaR(ndx_ts_5d$Stopy_zwrotu_5d[100:(dim(ndx_ts_5d)[1])],VaR_ndx_MC$VaR5_MC_norm, 0.05, Lags = 10))$LRuc
round(ndx_t.Christoffersena_MC_NORMALNY,3)
#Monte Carlo - Warunkowy rozklad t-Studenta
ndx_t.Christoffersena_MC_STUDENT <- (BacktestVaR(ndx_ts_5d$Stopy_zwrotu_5d[100:(dim(ndx_ts_5d)[1])],VaR_ndx_MC$VaR5_MC_student, 0.05, Lags = 10))$LRuc
round(ndx_t.Christoffersena_MC_STUDENT,3)





# NKX
# Warunkowy rozklad Normalny
nkx_t.Christoffersena_NORMALNY <- (BacktestVaR(nkx_ts_5d$Stopy_zwrotu_5d[101:(dim(nkx_ts_5d)[1])],na.omit(nkx_ts_5d$VaR5_norm), 0.05, Lags = 10))$LRuc
round(nkx_t.Christoffersena_NORMALNY,3)
# Warunkowy rozklad t-Studenta
nkx_t.Christoffersena_STUDENT <- (BacktestVaR(nkx_ts_5d$Stopy_zwrotu_5d[101:(dim(nkx_ts_5d)[1])],na.omit(nkx_ts_5d$VaR5_student), 0.05, Lags = 10))$LRuc
round(nkx_t.Christoffersena_STUDENT,3)
#######
#Monte Carlo - Warunkowy rozklad Normalny
nkx_t.Christoffersena_MC_NORMALNY <- (BacktestVaR(nkx_ts_5d$Stopy_zwrotu_5d[100:(dim(nkx_ts_5d)[1])],VaR_nkx_MC$VaR5_MC_norm, 0.05, Lags = 10))$LRuc
round(nkx_t.Christoffersena_MC_NORMALNY,3)
#Monte Carlo - Warunkowy rozklad t-Studenta
nkx_t.Christoffersena_MC_STUDENT <- (BacktestVaR(nkx_ts_5d$Stopy_zwrotu_5d[100:(dim(nkx_ts_5d)[1])],VaR_nkx_MC$VaR5_MC_student, 0.05, Lags = 10))$LRuc
round(nkx_t.Christoffersena_MC_STUDENT,3)




# DAX
# Warunkowy rozklad Normalny
dax_t.Christoffersena_NORMALNY <- (BacktestVaR(dax_ts_5d$Stopy_zwrotu_5d[101:(dim(dax_ts_5d)[1])],na.omit(dax_ts_5d$VaR5_norm), 0.05, Lags = 10))$LRuc
round(dax_t.Christoffersena_NORMALNY,3)
# Warunkowy rozklad t-Studenta
dax_t.Christoffersena_STUDENT <- (BacktestVaR(dax_ts_5d$Stopy_zwrotu_5d[101:(dim(dax_ts_5d)[1])],na.omit(dax_ts_5d$VaR5_student), 0.05, Lags = 10))$LRuc
round(dax_t.Christoffersena_STUDENT,3)
#######
#Monte Carlo - Warunkowy rozklad Normalny
dax_t.Christoffersena_MC_NORMALNY <- (BacktestVaR(dax_ts_5d$Stopy_zwrotu_5d[100:(dim(dax_ts_5d)[1])],VaR_dax_MC$VaR5_MC_norm, 0.05, Lags = 10))$LRuc
round(dax_t.Christoffersena_MC_NORMALNY,3)
#Monte Carlo - Warunkowy rozklad t-Studenta
dax_t.Christoffersena_MC_STUDENT <- (BacktestVaR(dax_ts_5d$Stopy_zwrotu_5d[100:(dim(dax_ts_5d)[1])],VaR_dax_MC$VaR5_MC_student, 0.05, Lags = 10))$LRuc
round(dax_t.Christoffersena_MC_STUDENT,3)



