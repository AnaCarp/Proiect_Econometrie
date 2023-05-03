#Aplicatia 1: Model de regresie

# Instalarea pachetelor
# Instalare si activare pachete
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap","ggplot2","DataCombine","car","tseries","readxl",
                  "foreign","caret","glmnet","corrplot","RColorBrewer","dplyr",
                  "plm","readxl","foreign")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

library("gplots")
library("readxl")
library("plm")
library("foreign")
library("dplyr")  
library("tidyverse")
library("stargazer")
library("magrittr")
library("lmtest")

library("olsrr")
library("moments")
library("whitestrap")
library("sandwich")
library("tseries")
library("readxl")
library("foreign")
library("tidyverse")
library("glmnet")
library("corrplot")
library("RColorBrewer")
library("caret")
library("caTools")
library(dplyr)
library(DataCombine)

#clean environment
rm(list = ls())

#citire date
data <-read.csv("D:\\FACULTATE\\AN 3\\SEMESTRUL 1\\Econometrie\\Proiect\\Rproject\\WorldEnergyConsumption.csv")



# Statistici descriptive
summary(data)

# Declararea setului de date de tip panel
pd.df <- pdata.frame(data, index = c("country","year"), drop.index = TRUE)

# Corelatia dintre gdp/spatiu/timp
coplot(gdp ~ year|country, type="l", data=data) 

# Heterogeneitatea presupune ca exista diferente intre unitatile studiate

# Explorarea heterogeneitatii in sectiunea transversala
# Graficul traseaza un interval de incredere de 95% in jurul mediilor
# Tari cu rata foarte mare si tari cu rata foarte mica => avem heterogeneitate transversala
plotmeans(gdp ~ country, main = 'Heterogeneitate in randul tarilor', data = data)


# Explorarea heterogeneitatii in sectiunea temporala
# Ani cu rata mare si ani cu rata mica => avem heterogeneitate temporala, 
# dar mai mica decat in cazul heterogeneitatii transversale
plotmeans(gdp ~ year, main = 'Heterogeneitate in timp', data = data)

# Model OLS - model clasic de regresie liniara
# Nu ia in calcul heterogeneitatea intre spatiu si timp
ols <- lm(gdp ~ biofuel_electricity +
          hydro_electricity +
          solar_electricity +
          wind_electricity +
          other_renewable_electricity, data)
summary(ols) #output


# Model FE (cu efecte fixe) 
fe <- plm(gdp ~ 
            biofuel_electricity +
            hydro_electricity +
            solar_electricity +
            wind_electricity +
            other_renewable_electricity
           , data, index = c('country','year'),
          model = 'within')
summary(fe)

#Eliminam coef nesemnificativi
fe <- plm(gdp ~ 
            wind_electricity +
            hydro_electricity 
            , data, index = c('country','year'),
          model = 'within')
summary(fe)

# Alegerea celei mai adecvate variante de model prin testarea intre regresie 
# OLS vs fixed effects panel model
# H0: FE 
# H1: OLS
pFtest(fe, ols) # p-value < 0.05 => se recomanda model de panel data FE

# Model cu efecte aleatorii RE (random effects)
re <- plm(gdp ~ 
            biofuel_electricity +
            hydro_electricity +
            solar_electricity +
            wind_electricity +
            other_renewable_electricity
          , data, index = c('country','year'),
          model = 'between')
summary(re)
re <- plm(gdp ~ 
            wind_electricity
          , data, index = c('country','year'),
          model = 'between')
summary(re)

# Testarea efectelor random 
pFtest(re, ols) # p-value > 0.05 => nu se recomanda efecte random.
plmtest(re, c('time'), type = 'bp') # p-value > 0.05 => nu se recomanda efecte random


# Testul Hausmann il utilizam pentru a decide intre FE si RE
# H0: model cu efecte random 
# H1: model cu efecte fixe
phtest(fe,re) # p-value < 0.05 => model cu efecte fixe se recomanda

# Testarea efectelor fixe in timp
fixed.time <- plm(gdp ~ 
                    wind_electricity +
                    hydro_electricity  +
                    factor(year), data=data, index=c("country","year"), model="within")

# H0:  nu sunt necesare efectele fixe in timp
# H1:  sunt necesare efectele fixe in timp
pFtest(fixed.time, fe) # p-value > 0.05 =>nu se recomanda folosirea efectelor fixe in timp
plmtest(fe, c('time'), type = 'bp') # p-value >0.05  => nu este nevoie sa se utilizeze efecte fixe in timp 


# Testarea dependentei transversale folosind testul Breusch-Pagan LM si testul Parasan CD

# Ipoteze teste
# H0: reziduurile intre entitati nu sunt corelate
# H1: reziduurile intre entitati sunt corelate

pcdtest(fe, test = 'lm') # p-value < 0.05 => dependenta transversala
pcdtest(fe, test = 'cd') # pvalue > 0.05 => nu avem dependenta transversala
# Nu corectam pt ca avem panel mic. daca aveam serie de timp 40 perioade + trebuia sa corectam

# Testarea autocorelarii - Breusch-Godfrey/Wooldridge test 
# Testul se aplica doar cand seriile de timp sunt lungi. In acest caz nu 
# pune probleme setul de date deoarece avem date pe 10 ani

# H0: Nu exista autocorelate
# H1: autocorelarea este prezenta
pbgtest(fe) # p-value < 0.05 => avem autocorelarem dar fiind panelul mic
# o vom ignora


# Testarea heteroschedasticitatii cu testul Breusch-Pagan
# H0: homoschedasticitate
# H1: heteroschedasticitate
bptest(gdp ~ 
         wind_electricity +
         hydro_electricity  +
         factor(country), data = data, studentize=F)
# deoarece p-value <0.05 => avem heteroschedasticitate


