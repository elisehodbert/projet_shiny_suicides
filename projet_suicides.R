require(data.table)
require(FactoMineR)
require(Factoshiny)
require(leaps)
require(leaflet)
require(glmnet)
require(boot)
require(pls)
require(missMDA)

# Importation des données
dta <- fread("GHSH_Pooled_Data1.csv")


str(dta)
dta$Country <- as.factor(dta$Country)
dta$Sex <- as.factor(dta$Sex)
dta$Age_Group <- as.factor(dta$Age_Group)
dta$Year <- as.factor(dta$Year)

# Description des variables

summary(dta)
plot(dta$Currently_Drink_Alcohol)
plot(dta$Really_Get_Drunk)

# ACP

#res <- Factoshiny(dta)


#AFM

data_afm <- data.frame(cbind(dta[,0:8], dta[,12], dta[,9:11], dta[,13:17]))
afm <- MFA(data_afm, 
           group=c(1, 1, 2,5, 7, 1), 
           type = c("n", "n", "n", "s", "s", "s"), 
           name.group = c("Pays", "Année", "Civilité", "Conso", "Sociale", "Suicide"), 
           num.group.sup = c(1,2,3,6)) #on met Pays et Suicide en variables supp

afmbis <- Factoshiny(afm)

#Premières remarques : suicide semble plus corrélé au sociale, qu'à la consommation



# Régression avec VARIABLES QUANTI
data_quanti <- data.frame(cbind(Attempted_suicide = data_afm$Attempted_suicide, data_afm[,5:16]))


#Estimation des données manquantes
ncomp <- estim_ncpPCA(data_quanti) #le nombre de composante nécessaire pour déterminer les NA, on va utiliser dans
#la fonction MIPCA
data_impute <- imputePCA(data_quanti, ncp = ncomp$ncp)
data_quanti_complet <- data.frame(data_impute$completeObs) 
which(is.na(data_quanti_complet)) #on a plus de NA

selection <- summary(regsubsets(data_quanti_complet$Attempted_suicide~., data=data_quanti_complet, nvmax= 12)) #on a mis 12 pcq on a 12
#variables explicatives 

### Boolean selection matrix for best submodels 
selection$which #pour choisir une variable explicative

### Best sub-model with one variable
colnames(selection$which)[selection$which[1,]]

### Best sub-model with two variables
colnames(selection$which)[selection$which[2,]]

#RSS plot
plot(1:12,selection$rss,type="b",pch=16,xlab="Number of variables in the submodel",
     ylab="Residual sum-of-squares",main="Quality of fit of best submodels",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2,col="darkgray",bty="l")
grid()


plot(1:12,selection$rsq,type="b",pch=16,xlab="Number of variables in the submodel",
     ylab=expression(R^2),main="Quality of fit of best submodels",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2,col="darkgray",bty="l")
grid()

##à partir de 8 variables dans le modèles, la qualité du modele n'augmente plus selon le critère RSS

#Best submodel with one explenatory variable
mod <- glm(Attempted_suicide~., data= data_quanti_complet[selection$which[1,]])
anova(mod)
### 10-fold CV PRESS 

cvmod = cv.glm(data = data_quanti_complet[,selection$which[1,]], mod, K=10) #cv.glm donne le msep

#comment enlever/ne pas prendre en compte les NA?
#packahe miss mda: fonction input.PCA
#quel K prendre? 10 c bien

cvmod$delta           # MSEP
nrow(data_quanti_complet)*cvmod$delta # PRESS
selection$rss[1]         # RSS

press = rep(0,12)     # vector of PRESS for best sub-models
for (j in 1:12) {
  mod = glm(Attempted_suicide~.,data=data_quanti_complet[,selection$which[j,]])
  cvmod = cv.glm(data_quanti_complet[,selection$which[j,]],mod,K=10)
  press[j] = nrow(data_quanti_complet)*cvmod$delta[2] 
}
#press: calcul des RSS pour chaque modèle (avec 1, 2 ... ou 12 variables prises en compte)

# PRESS plot for exhaustive feature selection
plot(1:12,selection$rss,type="b",pch=16,xlab="Number of variables in the submodel",
     ylab="Residual sum-of-squares",main="Quality of fit of best submodels",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2,col="darkgray",bty="l")
lines(1:12,press,type="b",col="blue",pch=15,lwd=2)
legend("topright",lwd=2,pch=c(16,15),legend=c("Internal validation","Cross validation"),
       bty="n",cex=1.25,col=c("darkgray","blue"))
grid()

##meileurs modèles: RSS le plus petit soit ici un modèle avec 6 VARIABLES




#BIC AIC
bic = selection$bic                            # BIC
aic = bic - (log(nrow(data_quanti_complet))-2)*(1:12)       # AIC

plot(1:12,bic,pch=16,bty="l",type="b",xlab="Number of explanatory variables",
     ylab="Information criterion",ylim=range(c(aic,bic)),col="darkgray",
     main="Exhaustive model selection",cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2)
lines(1:12,aic,type="b",pch=17,lwd=2,col="coral1")
legend("topleft",lwd=2,lty=1,pch=c(16,17),col=c("darkgray","coral1"),bty="n",cex=1.25,legend=c("BIC","AIC"))
grid()


selected = selection$which[which.min(bic),]   # Indices of selected variables (+ LMP)
bestmod = glm(Attempted_suicide~.,data=data_quanti_complet[,selected])   # Fits the best submodel
coef(bestmod)


## Prediction performance of the best submodel (minimizing AIC)

n = nrow(data_quanti_complet)                   # Sample size
segments = cvsegments(k=10,N=n) # Defines a list of 10 random segments
segments

cvpredictions = rep(0,n)   # Initialize a n-vector of predicted LMP 
for (k in 1:10) {
  train = data_quanti_complet[-segments[[k]],]   # Training dataset
  test = data_quanti_complet[segments[[k]],]     # Test dataset
  select = summary(regsubsets(Attempted_suicide~.,data=train,nvmax=11))
  bic = select$bic                            # BIC
  selected = select$which[which.min(bic),]   # Indices of selected variables (+ LMP)
  bestmod = glm(LMP~.,data=train[,selected])   # Fits the best submodel
  cvpredictions[segments[[k]]] = predict(bestmod,newdata=test)
}

PRESS = sum((pig$LMP-cvpredictions)^2)

# PRESS plot for exhaustive feature selection
plot(1:11,press,type="b",pch=16,xlab="Number of variables in the submodel",
     ylab="Residual sum-of-squares",main="Quality of fit of best submodels",
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25,lwd=2,col="darkgray",bty="l")
abline(h=PRESS,col="blue",pch=15,lwd=2)
legend("topright",lwd=2,pch=c(16,16),legend=c("Best submodels","Best submodel"),
       bty="n",cex=1.25,col=c("darkgray","blue"))
grid()

# Regression with a K-class response







#Variables quali
mod.nul <- lm(Attempted_suicide ~1, dta)
mod.country <- lm(Attempted_suicide ~ Country, dta)
Anova(mod.country)
mod.sexe <- lm(Attempted_suicide ~ Sex, dta)
Anova(mod.sexe)
mod.Age <- lm(Attempted_suicide ~ Age_Group, dta)
Anova(mod.Age)
mod.Year <- lm(Attempted_suicide ~ Year, dta )
Anova(mod.Year)


mod.comb.coun.sexe <- lm(Attempted_suicide ~ Country + Sex, dta)
Anova(mod.comb.coun.sexe)
mod.comb.coun.Age <- lm(Attempted_suicide ~ Country + Age_Group, dta)
Anova(mod.comb.coun.Age)


### MSEP for the Lasso estimator

## Multivariate model

### Matrix of explanatory variables
x = as.matrix(data_afm[,-17])

### Vector of response values
y = data_afm[,17]


loglambda = seq(10,-10,length=100)
#### Initialize an empty matrix of predicted values
cvpred = matrix(0,nrow=nrow(data_afm),ncol=100) 

#### Create 10 random segments of the dataset
segments = cvsegments(nrow(data_afm),10) 

for (k in 1:10) {
  trainx = x[-segments[[k]],]
  trainy = y[-segments[[k]]]
  testx = x[segments[[k]],]
  cvmod = cv.glmnet(trainx,trainy,alpha=1,lambda=exp(loglambda))
  mod = glmnet(trainx,trainy,alpha=1,lambda=exp(loglambda))
  cvpred[segments[[k]],] = predict(mod,newx=testx)[,which.min(cvmod$cvm)] 
  print(paste("Segment ",k,sep=""))
}

MSEP = mean((data_afm$Attempted_suicide-cvpred)^2)

## Searching for the best penalty parameter
cvmod = cv.glmnet(x,y,alpha=1,lambda=exp(loglambda))

## MSEP profile in lasso regression
plot(cvmod,ylim=c(1,5))
abline(h=MSEP,col="orange",lwd=2)


#########################################################################
#################### Cartographie #######################################
#########################################################################
# idée : ne pas centrer sur l'Europe

require(leaflet)
require(sp)
library(tidyverse)
library(leaflet)
library(readxl)
library(leaflet.providers)
library(raster)
library(sf)
library(geojsonio)

### 1. Colorer tous les pays qu'on a étudiés

remotes::install_github("geocompr/geocompkg")
library(terra) # classes and function for raster data 
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
library(colorspace)

World <- geojsonio::geojson_read("C:/Users/victo/OneDrive/Documents/GitHub/projet_suicides/ne_10m_admin_0_scale_rank_minor_islands-20221005T085330Z-001/ne_10m_admin_0_scale_rank_minor_islands.shp", what = "sp")
plot(World)

# Fonction pour garder uniquement les pays qui nous intéressent
# difficile car c'est un spatial polygon dataframe
# je n'ai pas encore trouvé la solution
# plan B : j'attribue une valeur aux pays qui n'ont pas de données
# et je les colore en gris ensuite

World@data[["sr_subunit"]] <-as.factor(World@data[["sr_subunit"]])
all_countries <- World@data[["sr_subunit"]]
select_countries <- data_afm$Country

# fonction pour construire une colonne avec les bonnes valeurs pour 
# nos pays et -9 pour les autres pays
# la fonction prend aussi en arguments j qui est la colonne de data_afm 
# contenant la valeur désirée

constr_col <- function(x){
  if (x %in% select_countries){
    valeur = 1
  }
  else{
    valeur = 0
  }
  return (valeur)
}

col_countries <- sapply(all_countries,constr_col)

# Création de la carte

factpal <- colorFactor(topo.colors(2), col_countries)

Carte1 <- leaflet(World) %>% 
  addTiles() %>%
  addPolygons(stroke = FALSE, 
              smoothFactor = 0.3, 
              fillOpacity = 1,
              fillColor = ~pal(col_countries), 
              popup=~World@data[["sr_subunit"]])
Carte1


### 2. Afficher les résultats en mettant la variable qu'on veut sur factoshiny

# Variables qui seront mises par l'utilisateur sur Factoshiny
Country <- as.factor("Argentina")
Sexe <- as.factor("Female")
Age <- as.factor("13-15")
Year <- as.factor("2018")
Var_qti <- "Attempted_Suicide"

constr_col2 <- function(x){ # fonction à revoir
  if (x %in% Country){
    valeur = data_afm[which(data_afm$Country == x),17]
  }
  else{
    valeur = "0"
  }
  return (valeur)
}
col_countries2 <- sapply(all_countries,constr_col2)

Carte2 <- leaflet(World) %>% 
  addTiles() %>%
  addPolygons(stroke = FALSE, 
              smoothFactor = 0.3, 
              fillOpacity = 1,
              fillColor = ~pal(col_countries2),
              popup=~paste("Pays : ",
                           World@data[["sr_subunit"]],
                           "<br>",
                           "Taux de tentatives de suicides : ",
                           "<br>",
                           col_countries2))
Carte2

#brouillon
WorldCountry <-geojsonio::geojson_read("C:/Users/elise/OneDrive/Documents/ACO_M2Stats/Projet_suicides/world.geo.json-master/countries.geo.json", what = "sp")
Country <- unique(data_afm$Country)
Valeur <- rep(1,length(Country))
data_carte1 <- data.frame(Country,Valeur)

Carte1 <- leaflet(WorldCountry) %>% 
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop)))
Carte1

labels <- sprintf(
  "<strong>%s</strong><br/>%g projects <sup></sup>",
  data_carte1$Country, data_carte1$Valeur) %>% lapply(htmltools::HTML)
labels

Carte1 %>% addPolygons(
  fillColor = ~pal(data$Valeur),
  weight = 1,
  opacity = 1,
  color = 'black',
  dashArray = '3',
  fillOpacity = 0.7,
  #label = labels,
)

### 3. En rentrant les vars du modèle, afficher le résultat par pays (si on a un effet du)


