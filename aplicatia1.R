#Aplicatia 1: Model de regresie

# Instalarea pachetelor
# Instalare si activare pachete
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap","ggplot2","DataCombine","car","tseries","readxl",
                  "foreign","caret","glmnet","corrplot","RColorBrewer","dplyr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


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
movies <-read.csv("D:\\An3Sem1\\Econometrie\\Proiect\\Rproject\\movies.csv")

#2. Data cleaning

#Verificare valori repetate
nrow(movies)
## [1] 7668
nrow(unique(movies))
## [1] 7668

#Eliminare NA 
movies_updated<-na.omit(movies)
movies_updated<-movies_updated%>%mutate(s_gross=gross/1000000, s_budget=budget/1000000)

#Eliminare outlieri si selectie date


movies_updated2 <- movies_updated %>%
  filter(s_budget < 100)
movies_updated3 <- movies_updated2 %>%
  filter(s_budget < 75)
movies_updated4 <- movies_updated3 %>%
  filter(s_budget < 50)

movies_updated5 <-movies_updated4 %>%
  filter(year > 2010 ) %>%
  filter(genre == 'Comedy' )


# Basic box plot
p <- ggplot(movies_updated4, aes(x=s_gross, y=s_budget)) + 
  geom_boxplot()
print(p)


######################### Grafice #########################

group_mean_budget <- aggregate(s_budget~year, data =movies_updated5 , mean)
print(group_mean_budget)
group_mean_gross <- aggregate(s_gross~year, data =movies_updated5 , mean)
print(group_mean_gross)

p<-ggplot()+
  geom_line(data= group_mean_budget, mapping = aes(x=year , y=s_budget),color="blue")+
  geom_point(data= group_mean_budget, mapping = aes(x=year , y=s_budget),color="blue")+
  geom_line(data= group_mean_gross, mapping = aes(x=year , y=s_gross),color="red")+
  geom_point(data= group_mean_gross, mapping = aes(x=year , y=s_gross),color="red")
print(p)


group_dif_profit <- aggregate(s_gross~year, data =movies_updated5 , mean)
print(group_dif_profit)
colnames(group_dif_profit) <- c('year','pr')
Year <- group_dif_profit$year
Profit <- group_dif_profit$pr
p2<-barplot(Profit,names.arg=Year,xlab="Year",ylab="Profit",col="blue",
            main="Profit chart",border="red")
print(p2)

######################################### Model de regresie unifactorial ######################################### 

movies_updated5<-movies_updated5%>%mutate(s_gross=gross/1000000, s_budget=budget/1000000)
model_0<-lm(s_gross~s_budget, data=movies_updated5)
summary(model_0)


movies_updated5%<>% mutate(uhat = resid(model_0)) 

# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 


# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model_0) > (model_0$rank - 1)

# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru


# Ipoteza 4 - Variabilitatea in x este pozitiva
var(movies_updated5$s_gross) #  > 0 => ipoteza acceptata


# Ipoteza 5 - Media reziduurilor este 0
mean(model_0$residuals) # medie aproape de 0 => ipoteza acceptata


# Ipoteza 6 - Testare multicoliniaritate => nu testam pt modele unifactorial

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(movies_updated5$s_budget, model_0$residuals) # p-value=1 > 0.1 => nu sunt corelate
# => ipoteza acceptata



# Ipoteza 8 - Reziduurile sunt homoscedastice
model_0<-lm(s_gross~s_budget, data=movies_updated5)
summary(model_0)

bptest(model_0) # pvalue<0.05 hetero
white_test(model_0)  #pvalue<0.05 hetero



# WLS: estimam modelul cu ponderea weight=1/sqrt(s_budget)
model_WLS1 <- lm(formula = s_gross ~ s_budget, 
                 data = movies_updated5, weights = 1/sqrt(s_budget))


movies_updated5 %<>% mutate(s_grossstar = s_gross/sqrt(s_budget),
                        s_budgetstar = s_budget/sqrt(s_budget),
                        conststar = 1/sqrt(s_budget))

model_WLS2 <- lm(s_grossstar ~ 0 +conststar+s_budgetstar,data = movies_updated5) 


bptest(model_WLS2) 
white_test(model_WLS2)


summary(model_WLS2)



# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(model_WLS2$residuals) # nu sunt autocorelate
dwtest(model_WLS2) # p-value < 0.1 => reziduuri autocorelate 
bgtest(model_WLS2) # p-value < 0.1 => reziduuri autocorelate 
# ipoteza respinsa, corectam:



# Cream un nou set de date 
movies_updated6 <- data.frame(movies_updated5, resid_modelWLS2=model_WLS2$residuals)
# Cream variabila lag1 
movies_updated6_1 <- slide(movies_updated6, Var="resid_modelWLS2", NewVar = "lag1", slideBy = -1)
movies_updated6_2 <- na.omit(movies_updated6_1) 
model3 <- lm(s_grossstar ~ 0 +conststar+s_budgetstar+lag1, data=movies_updated6_2)

# Retestam ipoteza pe noul model
# ACF 
acf(model3$residuals) # autocorelarea a disparut
# Durbin Watson 
dwtest(model3) # p-value > 0.1 => reziduuri nonautocorelate 
# Breusch-Godfrey 
bgtest(model3) # p-value > 0.1 => reziduuri nonautocorelate 
bgtest(model3, order = 2)  # p-value > 0.1 => reziduuri nonautocorelate 

dev.off()
# Ipoteza 10 -  Reziduurile sunt normal distribuite
jarque.bera.test(model3$residuals) # pvalue<0.05 =>reziduurile nu sunt normale distribuite
# => corectie
ols_plot_cooksd_bar(model3)


movies_updated7 <- movies_updated6_2[-c(30,31,89,101,117,131,147,148,177,216,217), ]
model4 <- lm(s_grossstar ~ 0 +conststar+s_budgetstar+lag1, data=movies_updated7)
jarque.bera.test(model4$residuals) 
ols_plot_cooksd_bar(model4)


#nu avem normalitate in date

summary(model4)

######## Prognoza pe interval de incredere ########

new_data <- data.frame(s_budgetstar=5.2,conststar=0,lag1=0)
predict(model4, newdata = new_data,interval = "confidence")

#Valoarea estimata a profitului pentru un buget de 5.2 mil $ este de 11.5 mil $
#Profitul se va incadra in intervalul de incredere este [9.84 mil $, 13.17 mil $ ] pentru un nivel de incredere de 95%

######################################### Model de regresie multiplu ######################################### 

movies_updated7<-movies_updated7%>%mutate(s_votes=votes/1000000)


mmodel1 <- lm(s_grossstar ~ 0 +conststar+s_budgetstar+s_votes+lag1, data=movies_updated7)
summary(mmodel1)


# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 


# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(mmodel1) > (mmodel1$rank - 1)

# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru


# Ipoteza 4 - Variabilitatea in x este pozitiva
var(movies_updated7$s_gross) #  > 0 => ipoteza acceptata
var(movies_updated7$s_votes)

# Ipoteza 5 - Media reziduurilor este 0
mean(mmodel1$residuals) # medie aproape de 0 => ipoteza acceptata


# Ipoteza 6 - Testare multicoliniaritate
vif(mmodel1) # nu avem valori pt VIF > 10 => ipoteza acceptata


# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(movies_updated7$s_budget, mmodel1$residuals) # p-value=1 > 0.1 => nu sunt corelate
cor.test(movies_updated7$s_votes, mmodel1$residuals)
# => ipoteza acceptata


# Ipoteza 8 - Reziduurile sunt homoscedastice

bptest(mmodel1) # pvalue>0.05 homo
white_test(mmodel1)  #pvalue>0.05 homo
#=>ipoteza acceptata


# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(mmodel1$residuals) # nu sunt autocorelate
dwtest(mmodel1) # p-value > 0.1 => reziduuri non - autocorelate 
bgtest(mmodel1,2) # p-value > 0.1 => reziduuri non - autocorelate 
#=>ipoteza acceptata

dev.off()
# Ipoteza 10 -  Reziduurile sunt normal distribuite
jarque.bera.test(mmodel1$residuals) # pvalue<0.05 =>reziduurile nu sunt normale distribuite
# => corectie
ols_plot_cooksd_bar(mmodel1)


movies_updated8 <- movies_updated7[-c(18,21,36,76,125,183), ]
mmodel3 <- lm(s_grossstar ~ 0 +conststar+s_budgetstar+s_votes+lag1, data=movies_updated8)
jarque.bera.test(mmodel3$residuals) 
ols_plot_cooksd_bar(mmodel3)


#nu avem normalitate in date
summary(mmodel3)


######################################### Extindere Model de regresie multiplu ######################################### 
############################  variabila dummy + termen interactiune + alta forma functionala  ##########################


later_than_2018<- ifelse(movies_updated8$year >= 2018, 1, 0) #variabila dummy pe an, folosim ca termen de interactiune cu voturile

movies_updated9<-movies_updated8%>%mutate(
  log_gross = log(s_grossstar),
  log_budget=log(s_budgetstar), 
  later_than_2018,
  imdbXlater_than_2018=score*later_than_2018)


extended_model <- lm(log_gross ~ 0+conststar+s_budgetstar+s_votes+imdbXlater_than_2018+lag1, data=movies_updated9)
summary(extended_model)


#Testarea ipotezelor modelului de regresie

# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 


# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(extended_model) > (extended_model$rank - 1)

# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru


# Ipoteza 4 - Variabilitatea in x este pozitiva
var(movies_updated9$log_budget) #  > 0 => ipoteza acceptata
var(movies_updated9$s_votes)
var(movies_updated9$imdbXlater_than_2018)

# Ipoteza 5 - Media reziduurilor este 0
mean(extended_model$residuals) # medie aproape de 0 => ipoteza acceptata


# Ipoteza 6 - Testare multicoliniaritate
vif(extended_model) # nu avem valori pt VIF > 10 => ipoteza acceptata


# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(movies_updated9$log_budget, extended_model$residuals) # p-value=1 > 0.1 => nu sunt corelate
cor.test(movies_updated9$s_votes, extended_model$residuals)
cor.test(movies_updated9$imdbXlater_than_2018, extended_model$residuals)
# => ipoteza acceptata


# Ipoteza 8 - Reziduurile sunt homoscedastice

bptest(extended_model) # pvalue<0.05 hetero #=>ipoteza respinsa
white_test(extended_model)  #pvalue>0.05 homo #acceptam ipoteza la limita


# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(extended_model$residuals) # nu sunt autocorelate
dwtest(extended_model) # p-value > 0.1 => reziduuri non - autocorelate 
bgtest(extended_model) # p-value > 0.1 => reziduuri non - autocorelate 
bgtest(extended_model,2) # p-value > 0.1 => reziduuri non - autocorelate 
#=>ipoteza acceptata


# Ipoteza 10 -  Reziduurile sunt normal distribuite
jarque.bera.test(extended_model$residuals) # pvalue<0.05 =>reziduurile nu sunt normale distribuite
# => corectie
ols_plot_cooksd_bar(extended_model)



movies_updated10 <- movies_updated9[-c(1,22,24,29,36,126,160),]
extended_model2 <- lm(s_grossstar ~ 0 +conststar+log_budget+s_votes+imdbXlater_than_2018+lag1, data=movies_updated10)
jarque.bera.test(extended_model2$residuals) 
ols_plot_cooksd_bar(extended_model2)


#nu avem normalitate in date
summary(extended_model2)

######## Prognoza pe interval de incredere
new_data <- data.frame(log_budget=0.8958797,s_votes=0.0220,imdbXlater_than_2018=0,conststar=0.4082483,lag1=-7.81978124)
predic <- predict(extended_model2, newdata = new_data,interval = "confidence")
exp(predic) 

#Valoarea estimata a profitului este de 782.4016 mil $ pentru 2.2 mii de voturi, pentru un film inainte de 2018(imdbXlater_than_2018=0) si o rata a bugetului de 0.895
#Profitul se va incadra in intervalul de incredere este [157.25 mil $, 3892.86 mil $ ] pentru un nivel de incredere de 95%

######################################## Prognoza pe baza de simulare pentru perioadele urmatoare ############################

# Prelucrarea datelor
df <-movies_updated10 %>%
  select(conststar,`log_budget`,`s_votes`,`imdbXlater_than_2018`,s_grossstar,`lag1`)
print(df)

df<-df[,c("conststar","log_budget","s_votes","s_grossstar","imdbXlater_than_2018","lag1")]
print(df)


sample <- sample.split(df, SplitRatio = 0.9)
train.data  <- subset(df, sample == TRUE)
test.data   <- subset(df, sample == FALSE)

print(test.data)

# Modelul final 
model_final <- 
  lm(s_grossstar ~ 0 +conststar+log_budget+s_votes+imdbXlater_than_2018+lag1, data=movies_updated10)
summary(model_final)


# Predictia modelului pe setul de testare
y_pred <- predict(model_final, newdata = test.data)
y_pred


#Out of sample forecasting
out_of_sample <- data.frame(s_budget = c(24,20,48),
                            s_votes = c(0.0470,0.0260,0.09),
                            imdbXlater_than_2018 = c(7,6.2,5),
                            conststar = c(0.2041241,0.223,0.144),
                            lag1=c(-5.54067765,3.17295748,25.117))

#Transformare buget introdus pentru predictie conform transformarilor realizate in model 
# astfel:
#  s_budgetstar <- s_budget/sqrt(s_budget) (de la corectia heteroschedasticitatii prin WLS)
#  logaritmare log_budget <- log(s_bugetstar)
#Realizam aceste transformari pentru a putea compara datele pentru predictie cu datele din modelul final ce a suferit diferite transformari

#Realizam radical
out_of_sample2 <- out_of_sample %>%
  mutate(s_budgetstar = s_budget/sqrt(s_budget))
# Logaritmam
out_of_sample_log <- out_of_sample2 %>%
  mutate(log_budget = log(s_budgetstar))


# Deselectam variabilele de care nu avem nevoie
out_of_sample_log <- out_of_sample_log %>%
  select(-s_budget,-s_budgetstar)
print(out_of_sample_log)

# Prognoza
y_pred_outsample <- predict(model_final, out_of_sample_log)
y_pred_outsample
exp(y_pred_outsample)

#Pentru un buget de 24 de milioane de $, 4700 voturi, un scor imdb 7 iar filmul este realizat dupa 2018 obtinem un profit de 2.2 milioane de $

#Pentru un buget de 20 de milioane de $, 2000 voturi, un scor imdb 6,2 iar filmul este realizat dupa 2018 obtinem un profit de 0,21 milioane de $

#Pentru un buget de 48 de milioane de $, 9000 voturi, un scor imdb 5 iar filmul este realizat dupa 2018 obtinem un profit de 106,84 milioane de $

#########################################  Metode de penalizare ######################################### 


mmodel10 <- lm(s_gross ~ s_budget+runtime+s_votes+score, data=movies_updated10)
summary(mmodel10)


# Prelucrarea datelor
df <-movies_updated10 %>%
  select(s_gross,s_budget,runtime,s_votes,score)


# Selectare coloane din lista
df<-df[,c("s_gross","s_budget","runtime","s_votes","score")]
print(df)

df[] <- lapply(df, function(x) if(is.numeric(x)){
  scale(x, center=TRUE, scale=TRUE)
} else x)

print(df)
colSums(is.na(df))

# Impartirea datelor in set de antrenare si testare
set.seed(123)
print(df)

sample <- sample.split(df, SplitRatio = 0.8)
train.data  <- subset(df, sample == TRUE)
test.data   <- subset(df, sample == FALSE)


# Definirea matricei de variabile independente
x <- data.matrix(train.data[, c('s_budget', 'runtime', 's_votes', 'score')])
# Vectorul variabilei dependente
y <- train.data[,c("s_gross")]
y


# Crearea a 10 secvente pentru a putea testarea in 10 folduri
# a validarii incrucisate pentru a selecta valoarea optima pentru lambda 
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)


# Regresia Ridge
# Valoarea optima pentru lambda
cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try)
# Afisarea valorii
cv$lambda.min
# Graficul MSE -> lambda
plot(cv)


# Estimarea modelului
model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Afisarea coeficientilor modelului
coef(model)
# Graficul de regularizare
res <- glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Regresia Lasso
# Valoarea optima pentru lambda
set.seed(123) 
cv_lasso <- cv.glmnet(x, y, alpha = 1,lambda = lambdas_to_try)
# Valoarea optima pt lambda in contextul LASSO
cv_lasso$lambda.min
# Plot
plot(cv_lasso)

# Estimarea modelului 
model_lasso <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Afisarea coeficientilor
coef(model_lasso)
# Graficul de regularizare
res_lasso <- glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res_lasso, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Elastic net 
# Antrenarea datelor pentru a gasi modelul optim prin testari succesive
set.seed(123)
model_en <- train(
  `s_gross` ~., data = df, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

# Estimarea modelului Elastic net
model_enet <- glmnet(x, y, alpha = 0.5, standardize = FALSE)
# Graficul de rezularizare
plot(model_enet,xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

# Identificarea modelului optim
model_en$bestTune

# Identificarea variabilelor importante 
importance <- varImp(model_en)
importance
plot(importance)

# Coeficientii modelului final
coef(model_en$finalModel, model_en$bestTune$lambda)

#########################################  Model variabile Boruta ######################################### 

# Algoritmul Boruta
library(Boruta)
install.packages("Boruta")

set.seed(111)
# Estimarea algoritmului
boruta.bank_train <- Boruta(s_grossstar~., data = movies_updated11, doTrace = 2)
print(boruta.bank_train)

# Afisarea atributelor semnificative luand in cosiderare si variabilele 'tentative'
boruta.bank <- TentativeRoughFix(boruta.bank_train)
print(boruta.bank)

# Graficul de importanta a variabilelor
plot(boruta.bank, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.bank$ImpHistory),function(i)
  boruta.bank$ImpHistory[is.finite(boruta.bank$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.bank$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.bank$ImpHistory), cex.axis = 0.7)

# Afisarea variabilelor importante fara cele marcate 'tentative'
getSelectedAttributes(boruta.bank, withTentative = F)

#Variabile importante conform algoritmului Boruta:
#[1] "votes"           "budget"          "gross"           "s_gross"         "s_budget"       
#[6] "uhat"            "s_budgetstar"    "conststar"       "resid_modelWLS2" "s_votes"   

model_boruta <- lm(s_grossstar ~ 0 +conststar+s_budgetstar+s_votes, data=movies_updated11)
summary(model_boruta)
