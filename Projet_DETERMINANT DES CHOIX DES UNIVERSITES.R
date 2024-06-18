######################################### Importation base de donnée #########################################
rm(list=ls())
library(readxl)
data <- read_excel("C:/Users/Daniel/Downloads/Bases.xlsx",col_types = c("numeric", "text", "text", 
                           "text", "text", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric"))

attach(data)
View(data)
######################################### Importation base de donnée #########################################
str(data)
data$etablissement=as.factor(data$etablissement)
data$code_du_domaine=as.factor(data$code_du_domaine)
str(data)
levels(data$etablissement)
View(data)

######################################### Condification établissemnt #########################################
code_etab=rep(0,length(etablissement))
data=data.frame(data,code_etab)
attach(data)
#Groupe1
data$code_etab[etablissement=="Paris-Saclay"]="U0"
data$code_etab[etablissement=="Sorbonne Université"]="U0"
#Groupe2
data$code_etab[etablissement=="Université de Paris"]="U1"
data$code_etab[etablissement=="Aix-Marseille"]="U1"
#Groupe3
data$code_etab[etablissement=="Grenoble Alpes"]="U2"
data$code_etab[etablissement=="Strasbourg"]="U2"
#Groupe4
data$code_etab[etablissement=="Montpellier"]="U3"
data$code_etab[etablissement=="Lyon 1 - Claude Bernard"]="U3"
#Groupe5
data$code_etab[etablissement=="Toulouse 3 - Paul Sabatier"]="U4"
data$code_etab[etablissement=="Bordeaux"]="U4"

data$code_etab=as.factor(data$code_etab)#Conversion en factor
View(data)
######################################### Condification établissemnt #########################################



######################################### Condification Domaine de formation #########################################
code_domaine=rep(0,length(code_du_domaine))
data=data.frame(data,code_domaine)
attach(data)
#Groupe1
data$code_domaine[code_du_domaine=="DEG"]=0
data$code_domaine[code_du_domaine=="LLA"]=1
data$code_domaine[code_du_domaine=="MEEF"]=2
data$code_domaine[code_du_domaine=="SHS"]=3
data$code_domaine[code_du_domaine=="STS"]=4

data$code_domaine=as.factor(data$code_domaine)#Conversion en factor
View(data)
######################################### Condification Domaine de formation #########################################
######################################### Statistique descriptive #########################################
library(fBasics)
summary(data)
basicStats(data$poids_de_la_discipline)
basicStats(data$taux_dinsertion)
basicStats(data$emplois_cadre_ou_professions_intermediaires)
basicStats(data$emplois_stables)
basicStats(data$emplois_a_temps_plein)
basicStats(data$salaire_brut_annuel_estime)
######################################### Statistique descriptive #########################################
######################################### Histogramme #########################################
# 1 : Salaire brut annuel estime
hist(salaire_brut_annuel_estime,breaks=15,col="red",density=5,xlab="Durée (années)",ylab="Occurrences",
     main="Durée moyenne des études dans le pays X",ylim=c(0,200),tck=0.01)
box()
# breaks : nombre de barres
# density : barres vides (0) ou hachurées
# tck = 0.01 : longueur des graduations
# xlab & ylab : titre de l'axe des abscisses et ordonnées
# main : titre de l'histogramme
# col : couleur des barres - pour mettre d'autres couleurs
hist(Codification_situation,breaks=15,col="red",density=5,xlab="Durée (années)",ylab="Occurrences",
     main="Durée moyenne des études dans le pays X",ylim=c(100,500),tck=0.01)
box()

library(ggplot2)
#Histogramme et fonction de densité
#salaire_brut_annuel_estime
ggplot(data, aes(x=salaire_brut_annuel_estime))+
  geom_histogram(color="black", fill="lightblue",
                 linetype="dashed")
ggplot(data, aes(x=salaire_brut_annuel_estime)) + 
  geom_density(alpha=.2, fill="#FF6666") 

#Taux d'insertion
ggplot(data, aes(x=taux_dinsertion))+
  geom_histogram(color="black", fill="lightblue",
                 linetype="dashed")
ggplot(data, aes(x=taux_dinsertion)) + 
  geom_density(alpha=.2, fill="#FF6666") 

#poids de la discipline
ggplot(data, aes(x=poids_de_la_discipline))+
  geom_histogram(color="black", fill="lightblue",
                 linetype="dashed")

ggplot(data, aes(x=poids_de_la_discipline)) + 
  geom_density(alpha=.2, fill="#FF6666") 

#emplois à temps plein
ggplot(data, aes(x=emplois_a_temps_plein))+
  geom_histogram(color="black", fill="lightblue",
                 linetype="dashed")
ggplot(data, aes(x=emplois_a_temps_plein)) + 
  geom_density(alpha=.2, fill="#FF6666")

#emplois cadres ou intemédiaires
ggplot(data, aes(x=emplois_cadre_ou_professions_intermediaires))+
  geom_histogram(color="black", fill="lightblue",
                 linetype="dashed")
ggplot(data, aes(x=emplois_cadre_ou_professions_intermediaires)) + 
  geom_density(alpha=.2, fill="#FF6666")

#code_etab
ggplot(data) +
  aes(x = code_etab, y = taux_dinsertion) +
  geom_violin() +
  xlab("Université") +
  ylab("Taux d'insertion") +
  ggtitle("Répartition par taux d'insertion selon l'université")

ggplot(data) +
  aes(x = code_etab, y = poids_de_la_discipline) +
  geom_violin() +
  xlab("Université") +
  ylab("poids") +
  ggtitle("Répartition par poids selon l'université")
library(GGally)
ggcorr(data)
######################################### Histogramme #########################################
######################################### Estimation #########################################
library(foreign)
library(nnet) #utiliser pour faire une estimation multinomiale
library(stargazer)
#On vérifier une fois nos modalités
table(data$etablissement)
table(data$code_etab)

table(data$code_domaine)
table(data$code_du_domaine)
View(data)
#Définir une modalté de référence de ce fait on a l'université 2
data$etabRef = relevel(data$code_etab, ref="U2")
#estimation
modele = multinom(etabRef ~ poids_de_la_discipline
                   + taux_dinsertion
                   + emplois_a_temps_plein
                   + emplois_stables
                   + emplois_cadre_ou_professions_intermediaires
                   + Codification_situation
                   + code_domaine, data=data)
summary(modele)
modele2 <- step(modele)#regression logistique multinomiale
summary(modele2)
#Plot 
plot(modele2,which = 1)

#Significativité des coefficients
stargazer(modele, type="text", out="multi1.htm")

#Matrice de confusion
table(predict(modele2, newdata = data), data$etabRef)


install.packages("GGally")
install.packages("broom.helpers")
library(GGally)
library(broom.helpers)
ggcoef_multinom(
  modele2,
  exponentiate = TRUE
)
#effets marginaux 
library(effects)
plot(allEffects(modele2))
library(nnet)
library(Zelig)
library(margins)
library(margins)
marginal_effects(modele)
