###Importation des packages###
library(shiny)
library(FactoMineR)#AFM, AovSum
library(leaps) #bestmod
#require(glmnet)
#require(boot)
#require(pls)
require(missMDA) #gestion des données manquantes
require(car) #Anova
library(colourpicker)
library(shinydashboard)
library(leaflet)


###Importation du fichier###
dta <- read.table("~/GitHub/projet_suicides/GHSH_Pooled_Data1.csv", header = TRUE, sep = ";")
#Mises en facteur des variables qualitatives
dta$Country <- as.factor(dta$Country)
dta$Sex <- as.factor(dta$Sex)
dta$Age_Group <- as.factor(dta$Age_Group)
dta$Year <- as.factor(dta$Year)

###AFM###
#Mise au propre du jeu de données
data_afm <- cbind(dta[,0:8], "Smoke_cig_currently" = dta[,12], dta[,9:11], dta[,13:17])
#Réalisation de l'AFM
afm <- MFA(data_afm, 
           group = c(1, 1, 2,5, 7, 1), 
           type = c("n", "n", "n", "s", "s", "s"), 
           name.group = c("Pays", "Année", "Civilité", "Conso", "Sociale", "Suicide"), #Determination des groupes 
           num.group.sup = c(1,2,3,6)) #on met Pays, Année, Civilité et Suicide en groupe supplémentaire

###Variables qualitatives
#On teste l'effet des variables qualitatives sur la variable Attempted_suicide

#Variable Country
mod.country <- lm(Attempted_suicide ~ Country, dta)
Anova(mod.country)
#Il y a un effet significatif de la variable Country sur Attempted_suicide

#Variable Sexe
mod.sexe <- lm(Attempted_suicide ~ Sex, dta)
Anova(mod.sexe)
#Il n'y a pas d'effet significatif de la variable Country sur Attempted_suicide

#Variable Age
mod.Age <- lm(Attempted_suicide ~ Age_Group, dta)
Anova(mod.Age)
#Il n'y a pas d'effet significatif de la variable Age sur Attempted_suicide

#Variable Year
mod.Year <- lm(Attempted_suicide ~ Year, dta )
Anova(mod.Year)
#Il y a un effet significatif de la variable Year sur Attempted_suicide


###Modèles avec variables quantitatives et qualitatives ###
#Mise au propre du jeu de données, on enlève les donneés manquantes
data_quanti <- data.frame(cbind(Attempted_suicide = data_afm$Attempted_suicide, data_afm[,5:16]))
ncomp <- estim_ncpPCA(data_quanti) #le nombre de composante nécessaire pour déterminer les NA, on va utiliser dans la fonction MIPCA
data_impute <- imputePCA(data_quanti, ncp = ncomp$ncp)
data_quanti_complet <- data.frame(data_impute$completeObs)
data_complet <- data.frame(cbind(data_quanti_complet, Country = data_afm$Country, Year = data_afm$Year))


#Elaboration du modèle
#On ne peut pas étudier les deux varibles Country et Year en même temps car tous les pays ne présentent pas toutes les modalités pour la variable Year
#On décide donc de ne garder que la varible Country car c'est celle qui nous intéresse le plus et qui présente l'effet le plus significatif
#On part du modèle constituer de la variable Country et de toutes les variables quantitatives
#Ensuite on selectionne les variables pour ne garder que celles qui présentent un effet significatif sur la Attempted_suicide
#On utilise la fonction AovSum() pour les modèles
modele.complet.bis <- AovSum(Attempted_suicide ~ Country + Currently_Drink_Alcohol + Really_Get_Drunk + Overwieght + Use_Marijuana + Have_Understanding_Parents + Missed_classes_without_permssion + Had_sexual_relation + Smoke_cig_currently + Had_fights + Bullied + Got_Seriously_injured + No_close_friends, data_complet)
modele.complet.bis

modele.complet.bis2 <- AovSum(Attempted_suicide ~ Country + Currently_Drink_Alcohol + Overwieght + Use_Marijuana + Have_Understanding_Parents + Missed_classes_without_permssion + Had_sexual_relation + Smoke_cig_currently + Had_fights + Bullied + Got_Seriously_injured + No_close_friends, data_complet)
modele.complet.bis2

modele.complet.bis3 <- AovSum(Attempted_suicide ~ Country + Overwieght + Use_Marijuana + Have_Understanding_Parents + Missed_classes_without_permssion + Had_sexual_relation + Smoke_cig_currently + Had_fights + Bullied + Got_Seriously_injured + No_close_friends, data_complet)
modele.complet.bis3

modele.complet.bis4 <- AovSum(Attempted_suicide ~ Country + Overwieght + Use_Marijuana + Have_Understanding_Parents + Missed_classes_without_permssion + Had_sexual_relation + Had_fights + Bullied + Got_Seriously_injured + No_close_friends, data_complet)
modele.complet.bis4

modele.complet.bis5 <- AovSum(Attempted_suicide ~ Country + Overwieght + Use_Marijuana + Have_Understanding_Parents + Missed_classes_without_permssion + Had_sexual_relation + Had_fights + Bullied + Got_Seriously_injured, data_complet)
modele.complet.bis5

modele.complet.bis6 <- AovSum(Attempted_suicide ~ Country + Overwieght + Use_Marijuana + Missed_classes_without_permssion + Had_sexual_relation + Had_fights + Bullied + Got_Seriously_injured, data_complet)
modele.complet.bis6

modele.complet.bis7 <- AovSum(Attempted_suicide ~ Country + Overwieght + Missed_classes_without_permssion + Had_sexual_relation + Had_fights + Bullied + Got_Seriously_injured, data_complet)
modele.complet.bis7

modele.complet.bis8 <- AovSum(Attempted_suicide ~ Country + Missed_classes_without_permssion + Had_sexual_relation + Had_fights + Bullied + Got_Seriously_injured, data_complet)
modele.complet.bis8

modele.complet.bis9 <- AovSum(Attempted_suicide ~ Country + Missed_classes_without_permssion + Had_fights + Bullied + Got_Seriously_injured, data_complet)
modele.complet.bis9

#Avec cette méthode, le meilleur modèle est un modèle à 5 variables : Country, Missed_classes_without_permssion, Had_fights, Bullied, Got_Seriously_injured

#Estimation des coefficients 

modele.complet.bis9$Ttest[,1]
modele.complet.bis9$Ttest["(Intercept)",1]

#extraction des coefficients
coefficients_intercept <- t(data.frame(modele.complet.bis9$Ttest[,1][1]))
coefficients_pays <- t(data.frame(modele.complet.bis9$Ttest[,1][2:28]))
colnames(coefficients_pays) <- sapply(colnames(coefficients_pays), substr, start = 10, stop = 30)
coefficients_pays1 <- t(data.frame(coefficients_pays[,1:14]))
coefficients_pays2 <- t(data.frame(coefficients_pays[,15:27]))
coefficients_quanti <- t(data.frame(modele.complet.bis9$Ttest[,1][29:32]))


###Modèle avec variables quantitatives ###
#Modèle supplémentaire non-présenté

#Selection du meilleur modèle pour chaque nombre de variable explicative
selection <- summary(regsubsets(data_quanti_complet$Attempted_suicide ~ ., data = data_quanti_complet, nvmax = 12)) #on a mis 12 pcq on a 12

# Meilleur modèle avec une variable
colnames(selection$which)[selection$which[1,]]
mod1 <- glm(Attempted_suicide ~ ., data = data_quanti_complet[selection$which[1,]])
anova(mod1)

# Meilleur modèle avec deux variables
colnames(selection$which)[selection$which[2,]]
mod2 <- glm(Attempted_suicide ~ ., data = data_quanti_complet[selection$which[2,]])
anova(mod2)

#Meilleur modèle
bic = selection$bic  
selected = selection$which[which.min(bic),]   # Indices of selected variables (+ LMP)
bestmod = glm(Attempted_suicide ~ .,data = data_quanti_complet[,selected])   # Fits the best submodel
#On obtient un modèle avec 6 variables explicatives qui sont les suivantes :
coef(bestmod)

#Carte

#remotes::install_github("geocompr/geocompkg")
World <- geojsonio::geojson_read("~/GitHub/projet_suicides/ne_10m_admin_0_scale_rank_minor_islands/ne_10m_admin_0_scale_rank_minor_islands.shp", what = "sp")
World@data[["sr_subunit"]] <-as.factor(World@data[["sr_subunit"]])
all_countries <- World@data[["sr_subunit"]]
select_countries <- data_afm$Country





