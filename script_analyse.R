###IMPORT DE LA BASE DE DONNEES

library(questionr)
str(L1eco)
View(L1eco)

###Extraction des lignes et des colonnes

nrow(L1eco)
ncol(L1eco)
dim(L1eco)
names(L1eco)
DF<-L1eco

####Analyse univariée############
#################################

###Extraction des variables xi###
#################################


##Variable qualitative SEXE

#statistique descriptive de la variable qualitative "SEXE"#

DF$SEXE
unique(DF$SEXE)
levels(DF$SEXE)<-c("Femmes","Hommes")

freq(DF$SEXE)
table(DF$SEXE)

tab1<-(table(DF$SEXE))
tab1

prop.table(tab1)
prop.table(tab1)*100
TAB1<-prop.table(tab1)*100

#Représentation graphique de la variable qualitative "SEXE" en effectifs et pourcentage#

barplot(sort(tab1,decreasing = TRUE), 
main="Diagramme en Barres du nombre 
d'hommes et de femmes au sein de la population",
xlab="Genres", ylab="Effectifs", ylim=c(0,1500),col=c("dodgerblue","lightpink"),border=NA)

text(0.6759356,1046.3862  , "Effectif de 1236 (60.9 %) ",col="black",pos=1)
text(1.8982515,619.7205, "Effectif de 793 (39.1 %) ",col="black",pos=1)

#Représentation graphique de la variable qualitative "SEXE" en pourcentage des effectifs de la population#

barplot(sort(TAB1,decreasing = TRUE), main="Diagramme en Barres du nombre d'hommes et de 
        femmes au sein de la population",xlab="Genres", ylab="pourcentage", ylim=c(0,100),
        col=c("dodgerblue","lightpink"))

legend("topright",legend=c("M :Hommes","F :Femmes"), title= "Définition des modalités" ,
cex=0.5, text.font=4)

text(0.6553695 ,52.54918   , "Effectif de 1236 (60.9 %) ",col="black",pos=1)
text(1.8967488,35.03965, "Effectif de 793 (39.1 %) ",col="black",pos=1)


##Variable qualitative PCS

#statistique descriptive de la variable qualitative PCS#

DF$PCS
unique(DF$PCS)

is.factor(DF$PCS)    
                     #Commentaires:
                     #-----------------------------------------------------------------------------------
                     #Bien que la variable "PCS" soit qualitative ordinale, nous avons remarqé 
                     #qu'elle est codifiée numériquement en tant que "integer" de la sorte à 
                     #comporter une echelle allant de 1 à 4 afin de décrire la situation proféssionnelle 
                     #des parents des étudiants avec 1 pour "Défavorisée", 2 pour "Modeste",3 pour 
                     #"Favorisée" et 4 pour Très Favorisée. 
                       
#Nous allons donc introduire la fonction "cut" afin de transformer la variable "PCS" en Factor.
#puis en définissant le nombre de breaks et les labels nous allons assigner à chaque valeur numérique 
#la situation professionnelle des parents des étudiants.
#--------------------------------------------------------------------------------------------------------

cut(DF$PCS,breaks = 4,labels = c("défavorisé","modeste","Favorisé","Très Favorisé"))
Sit_Pro<-cut(DF$PCS,breaks = 4,labels = c("Défavorisée","Modeste","Favorisée","Très Favorisée"))

levels(Sit_Pro)
ordered(Sit_Pro)    #Ordination des modalités 


is.factor(Sit_Pro)  #nous vérifions si la variable a bien été transformée en "Factor" 

table(Sit_Pro)
tab2<-table(Sit_Pro)
tab2

freq(tab2)
prop.table(tab2)
prop.table(tab2)*100
TAB2<-prop.table(tab2)*100
TAB2

#Représentation graphique de la variable qualitative "PCS" en effectifs et pourcentage#

barplot(tab2,main="Diagramme en Barres de la situation proféssionnelle des parents des étudiants",
        xlab="situation professionnelle", ylab="Effectifs", ylim=c(0,1000),col=c("dodgerblue"),border=NA)

Graph_PCS<-barplot(tab2,main="Diagramme en Barres de la situation proféssionnelle des parents 
                   des étudiants",xlab="situation professionnelle", ylab="Effectifs", ylim=c(0,1000),
                   col=c("dodgerblue"),border=NA)

text(Graph_PCS,tab2+20,c("17.3%","25.1%","16.6%","40.9%"),cex=1)


##Variable qualitative "retard"

#statistique descriptive de la variable qualitative "retard"#

DF$retard
unique(DF$retard)
levels(DF$retard)<-c("Bac obtenu à temps ou à l'avance","Bac obtenu avec un retard d'une année",
                     "Bac obtenu avec un retard de deux ans")

table(DF$retard)
freq(DF$retard)

tab3<-table(DF$retard)
prop.table(tab3)
prop.table(tab3)*100

barplot(tab3)

#Représentation graphique de la variable qualitative "retard" en effectifs et pourcentage#

Graph_retard<-barplot(tab3, main="Diagramme en barres des effectifs des étudiants ayant obtenu 
                      le Bac par ordre de retards",xlab="Ordre de retards",xaxt="n",ylab="Effectifs"
                      ,ylim=c(0,1500),col=c("chartreuse2","yellow1","gray87"),border=NA)

legend(2.4,1500,legend=c("Bac obtenu à temps ou à l'avance","Bac obtenu avec un retard d'une année",
       "Bac obtenu avec un retard de deux ans"),fill=c("chartreuse2","yellow1","gray87")
       ,bty="n",border=NA)

text(Graph_retard,tab3+30,c("58.7%","33.7%","7.5%"),cex=1)


##Variable qualitative "Bac"

#statistique descriptive de la variable qualitative "Bac"#

DF$BAC
unique(DF$BAC)

freq(DF$BAC)
tab4<-table(DF$BAC)

prop.table(tab4)
prop.table(tab4)*100

#Représentation graphique de la variable qualitative "Bac" en effectifs et pourcentage#

#1)Barplot par défaut:

barplot(tab4)

#2)Barplot personnalisé:

tab4<-sort(tab4,decreasing = TRUE) #classification des modalités par order décroissant#

Graph_Bac<-barplot(tab4,main="Séries de Bac",xlab="Branches",ylab="Effectifs",ylim=c(0,1600),
                   col="dodgerblue",border =NA )
Graph_Bac

legend("topright",legend=c("ES:économie sociale","S:scientifique","STT:sciences et téchnologies tertiaires","L:littéraire","STI:sciences et technologies industrielle","SMS:sciences et technologies médicosociales"), title="Branches",cex=0.48, text.font=4)   

text(Graph_Bac,tab4+50,c("70.3%","24.1%","3.3%","1.5%","0.4%","0.3%"),cex=0.75)


##Variable qualitative "Type_Bac"

#statistique descriptive de la variable qualitative "Type_bac"#

DF$Type_bac
unique(DF$Type_bac)
freq(DF$Type_bac)

table(DF$Type_bac)
tab5<-table(DF$Type_bac)
tab5

prop.table(tab5)
prop.table(tab5)*100

#Représentation graphique de la variable qualitative "Type_bac" en pourcentage#

pie(tab5,labels=c("96%","4%"),main="Types de Bac obtenus par les étuduants",col=c("dodgerblue","gray87")
    ,border=NA)

legend("bottomleft",legend=c("Bac général ","Bac technologique"),fill=c("dodgerblue","gray87")
       ,cex=0.7,text.font=4,bty="n",border=NA)


##Variable qualitative "res_bac"

#statistique descriptive de la variable qualitative "res_bac"#

DF$res_bac
unique(DF$res_bac)
levels(DF$res_bac)<-c("Très Bien","Bien","Assez Bien","Passable","Passe au 2ème Groupe")
freq(DF$res_bac)

tab6<-table(DF$res_bac)
tab6

prop.table(tab6)
prop.table(tab6)*100
TAB6<-prop.table(tab6)*100
TAB6

#Représentation graphique de la variable qualitative "res_bac" en pourcentage#

Graph_res_bac<-barplot(TAB6,main="Diagramme en barres des proportions des mentions obtenues par les étudiants 
au 1er groupe d’épreuves du Bac en pourcentage de la population", xlab = "Mentions ", 
ylab = "Pourcentage", ylim =c(0,100),col="dodgerblue",border=NA)

Graph_res_bac

text(Graph_res_bac,TAB6+5,c("0.3%","8%","25.7%","47.6%","18.3%"),cex=1)


##Variable qualitative "repech"

#statistique descriptive de la variable qualitative "res_bac"#

DF$repech
unique(DF$repech)

ordered(DF$repech)
sort(DF$repech)
freq(DF$repech)

tab7<-table(DF$repech)
tab7

prop.table(tab7)
prop.table(tab7)*100
TAB7<-prop.table((tab7)*100)
TAB7

#Représentation graphique de la variable qualitative "repêchage" en pourcentage#

pie(TAB7, labels=c("81.6%","18.4%"),main = "Pourcentage des étudiants repêchés", 
    col = c("coral","chartreuse2"), border=NA)

legend("bottomleft",legend=c("Etudiants repêchés","Etudiants non repêchés"),fill=c("chartreuse2","coral")
       ,cex=0.7,text.font=4,bty="n",border=NA)

##Variable qualitative "Mention"

#statistique descriptive de la variable qualitative "Mention"#

DF$Mention
unique(DF$mention)

ordered(DF$Mention)
freq(DF$Mention)

tab8<-table(DF$Mention)
prop.table(tab8)
TAB8<-prop.table(tab8)*100
TAB8

pie(TAB8,labels=c("65.9%","34.1%"),main="pourcentage des etudiant qui ont obtenu une Mention", 
    col=c("gray87","chartreuse2"),border=NA)

legend(-1.25,-0.95,legend=c("Etudiants ayant obtenu une mention","Etudiants n'ayant pas obtenu une mention"),fill=c("chartreuse2","gray87")
       ,cex=0.7,text.font=4,bty="n",border=NA)


##Variable quantitative "Note_math"

#statistique descriptive de la variable quantitative "Note_math"#

install.packages("moments")
library("moments") #package pour le calcul kutosis et le skewness#
DF$Note_math
unique(DF$Note_math)
summary(DF$Note_math,na.rm=TRUE)
quantile(DF$Note_math,na.rm=TRUE)
range(DF$Note_math,na.rm=TRUE)
sd(DF$Note_math,na.rm=TRUE)
var(DF$Note_math,na.rm=TRUE)
skewness(DF$Note_math,na.rm=TRUE)
kurtosis(DF$Note_math,na.rm=TRUE)

table(DF$Note_math)
tab9<-table(DF$Note_math)
sort(tab9)

TAB9<-prop.table(tab9)

Variance_Math<-var(DF$Note_math,na.rm=TRUE)
Variance_Math

(sd(DF$Note_math,na.rm=T)/mean(DF$Note_math,na.rm=T))*100

#Représentation graphique de la variable quantitative "Note_math"#



hist(DF$Note_math,freq=FALSE,main="Histogramme des notes en mathématiques obtenues par les étudiants",xaxt="n",
yaxt="n","Densité",breaks=20,xlim=c(0,20),ylim=c(0,0.15),xlab="Notes en Mathématiques",ylab="Densités des effectifs",col="dodgerblue",border="white")
axis(1,at=seq(0,20,1),labels= seq(0,20,1))
axis(2,at=seq(0,0.12,0.020),labels=seq(0,0.12,0.020))

lines(density((DF$Note_math),na.rm=TRUE),col="red",lty=1,lwd=2)


##Variable qualitative "réussite"

#statistique descriptive de la variable qualitative "réussite"#

DF$reussite
unique(DF$reussite)
is.factor(DF$reussite)   #Commentaires:
#---------------------------------------------------------------------------------------------------------
#Bien que la variable "reussite" soit qualitative, nous avons remarqé 
#qu'elle est codifiée numériquement où la valeur 0 indique l'echec et la valeur 1 la réussite des
#étudiants. Nous allons donc introduire la fonction "cut" afin de transformer la variable "reussite" 
#en Factor en assignant les valeurs numériques 0 et 1 respectivement à l'échec et à la réussite 
#----------------------------------------------------------------------------------------------------------

cut(DF$reussite,breaks = 2,labels = c("echec","réussite"))
réussite<-cut(DF$reussite,breaks = 2,labels = c("echec","réussite"))

levels(réussite)
ordered(réussite)    #Ordination des modalités 


is.factor(réussite)  #nous vérifions si la variable a bien été transformée en "Factor" 

table(réussite)
tab9<-table(réussite)
tab9

freq(tab9)
prop.table(tab9)
prop.table(tab9)*100
TAB9<-prop.table(tab9)*100
TAB9

#Représentation graphique de la variable qualitative "réussite" en effectifs et pourcentage#


pie(TAB9,labels=c("42.5%","57.5%"),main="pourcentage des etudiants ayant réussi", 
    col=c("gray87","chartreuse2"),border=NA)

legend("topleft",legend=c("Etudiants ayant réussi","Etudiants ayant échoué"),
       fill=c("chartreuse2","gray87"),cex=0.7,text.font=4,bty="n",border=NA)




###### bivariée#######

install.packages("vcd")
library(vcd)
#### V Q sexe reussite #####
tab11<-table(réussite,DF$SEXE)            #### tableau de contingence
tab11
addmargins(tab11)                  #### table de contingence avec les effectifs marginaux
prop.table(margin.table(tab11,1))       #### freq marginale ligne
prop.table(margin.table(tab11,2))       #### freq marginale collone

prop.table(tab11,1)      #### distributiion conditionnelles ligne
prop.table(tab11,2)      #### distributions conditionelles colonne
chisq.test(tab11)                              #### Calcul de la statistique Khi
str(chisq.test(tab11))      ###### éléments que calcule la fonction chisq.test
length(tab11)
assocstats(tab11)                 #####calcul du V cramer
sqrt(tab11)

graphSexe_r<-barplot(tab11, main = "Pourcentage des étudiants hommes et femmes 
ayant réussi en Licence 1 Economie et Gestion",xlab="Sexe",ylab = "Résultat de réussite",
ylim = c(0,1600),col=c("gray87","chartreuse2"),border=NA)
legend("topleft",legend=c("réussite", "echec"),fill = c("chartreuse2","gray87"),bty="n",border=NA)

text(graphSexe_r,c(120,200),c("34,04%","56.22%"),font=2,cex=1)
text(graphSexe_r,c(500,900),c("65.95%","43.77%"),font=2,cex=1)
###### VQ PCS reussite#####

tab12<-table(réussite,Sit_Pro) #### tableau de contingence
tab12
addmargins(tab12)                 #### table de contingence avec les effectifs marginaux
prop.table(margin.table(tab12,1))                    #### freq marginale ligne
prop.table(margin.table(tab12,2))                    #### freq marginale collone
prop.table(tab12,1)                                             #### distributiion conditionnelles ligne
prop.table(tab12,2)                                            #### distributiion conditionnelles en colonne
chisq.test(tab12)                                                  #### Calcul de la statistique Khi 
####chisq > 5% donc ce n'est pas la peine de faire un Graph
str(chisq.test(tab12))    ###### éléments que calcule la fonction chisq.test 
length(tab12)
assocstats(tab12)        #####calcul du V cramer
sqrt(tab12)
#######VQ Retard reussite ######

tab13<-table(réussite,DF$retard)                      #### tableau de contingence
tab13
addmargins(tab13)                    #### table de contingence avec les effectifs marginaux
prop.table(margin.table(tab13,1))                #### freq marginale ligne
prop.table(margin.table(tab13,2))               #### freq marginale collone
prop.table(tab13,1)                                   #### distributiion conditionnelles ligne
prop.table(tab13,2)                               #### distributions conditionelles colonne
chisq.test(tab13)                              #### Calcul de la statistique Khi
str(chisq.test(tab13))    ###### éléments que calcule la fonction chisq.test
assocstats(tab13)        #####calcul du V cramer
sqrt(tab13)

graphres_ret<-barplot(tab13,main = "Pourcentage de réussite des étudiants 
par rapport au retard à l'obtention du Bac",xlab= "Ordre de ratards",
ylab = "nombre d'étudiants ayant réussi au Bac",ylim = c(0,1500),col=c("gray87","chartreuse2"),border=NA)
legend("topright",legend=c("réussite", "echec"),fill = c("chartreuse2","gray87"),bty="n",border=NA)

text(graphres_ret,c(200,200,60),c("36,91%","60%","75%"),font=2,cex=1)
text(graphres_ret,c(700,500,135),c("63,08%","40%","25%"),font=2,cex=1)

##### VQ BAC reussite####

tab14<-table(réussite,DF$BAC)                     #### tableau de contingence
addmargins(tab14)                  #### table de contingence avec les effectifs marginaux
prop.table(margin.table(tab14,1))                #### freq marginale ligne
prop.table(margin.table(tab14,2))               #### freq marginale collone
prop.table(tab14,1)                                   #### distributiion conditionnelles ligne
prop.table(tab14,2)                         #### distributions conditionelles colonne
chisq.test((tab14))                              #### Calcul de la statistique Khi
str(chisq.test(tab14))    ###### éléments que calcule la fonction chisq.test
assocstats(tab14)        #####calcul du V cramer
sqrt(tab14)

graphbac_res<-barplot(tab14,legend.text=TRUE,main="Pourcentage des étudiants ayant réussi 
en L1 Economie et Gestion selon la série du Bac obtenu",xlab = "Séries du Bac",
ylab="Réussite et Echec",ylim = c(0,1600),col = c("gray87","chartreuse2"),border=NA)
legend(4,1000,legend=c("ES: Economie sociale ","S: Scientifique","STT: Sciences et Technologies Tertiaires","L: Lettre ","STI: Sciences et Technologies Industrielles","SMS: en Sciences et techniques Médico-Sociales ") ,
cex=0.7,text.font=4,bty = "n")
text(graphbac_res,c(300,50,100,30,30,20),c("49.26%","100%","31,49%","100%","100%","92.53%"),font=2,cex=1)
text(graphbac_res,c(1000,10,300,10,60,90),c("50.07%","","68.50%","","","7.46"),font=2,cex=1)

###### VQ type Bac reussite ######
tab15<-table(réussite,DF$Type_bac)                    #### tableau de contingence
addmargins(tab15)                          #### table de contingence avec les effectifs marginaux
prop.table(margin.table(tab15,1))                #### freq marginale ligne
prop.table(margin.table(tab15,2))               #### freq marginale collone
prop.table(tab15,1)                                   #### distributiion conditionnelles ligne
prop.table(tab15,2)                         #### distributions conditionelles colonne
chisq.test((tab15))                              #### Calcul de la statistique Khi
str(chisq.test(tab15))    ###### éléments que calcule la fonction chisq.test
assocstats(tab15)        #####calcul du V cramer
sqrt(tab15)

graphtbac<-barplot(tab15, main = "Etudiants ayant la reussi en L1 Economie et Gestion  
selon le type de Bac",xlab="Types de Bac ",ylab = "Réussite et Echec",ylim = c(0,2000),
col=c("gray87","chartreuse2"),border=NA)
legend("topright",legend=c("réussite", "echec"),fill = c("chartreuse2","gray87"),bty="n",border=NA)
text(graphtbac,c(500,30),c("45.60%","93.90%"),font=2,cex=1)
text(graphtbac,c(1450,110),c("54.39%","6.09%"),font=2,cex=1)

##### VQ res bac reussite #####

tab16<-table(réussite,DF$res_bac) #### tableau de contingence
tab16
addmargins(tab16)                  #### table de contingence avec les effectifs marginaux
prop.table(margin.table(tab16,1))                #### freq marginale ligne
prop.table(margin.table(tab16,2))               #### freq marginale collone
prop.table(tab16,1)                                   #### distributiion conditionnelles ligne
prop.table(tab16,2)                         #### distributions conditionelles colonne
chisq.test((tab16))                              #### Calcul de la statistique Khi
str(chisq.test(tab16))             ##### éléments que calcule la fonction chisq.test
assocstats(tab16)        #####calcul du V cramer
sqrt(tab16)

graphbac<-barplot(tab16,main="Etudiants ayant réussi en L1 Economie et Gestion
selon la mention obtenue au Bac",xlab = "Mentions Obtenues au Bac",ylab="Echec et Reussite",
ylim = c(0,1500),col = c("gray87","chartreuse2"),border=NA )
legend("topright",legend=c("réussite", "echec"),fill = c("chartreuse2","gray87"),bty="n",border=NA)
text(graphbac,c(20,20,20,300,150),c("","7.97%","15.16%","60,76%","76,88%"),font=2,cex=1)
text(graphbac,c(30,130,250,800,320),c("100%","92.02%","84,83%","39.23%","23.11%"),font=2,cex=1)

#### VQ repech reussite######

repch<-factor(DF$repech,levels = c("N","O"),labels = c("Etudiants repêchés"," Etudiants non Repêchés"))
tab17<-table(réussite,repch)                  #### tableau de contingence
tab17
addmargins(tab17)                  #### table de contingence avec les effectifs marginaux
prop.table(margin.table(tab17,1))                #### freq marginale ligne
prop.table(margin.table(tab17,2))               #### freq marginale collone
prop.table(tab17,1)                                   #### distributiion conditionnelles ligne
prop.table(tab17,2)                         #### distributions conditionelles colonne
chisq.test((tab17))                              #### Calcul de la statistique Khi
str(chisq.test(tab17))        ###### éléments que calcule la fonction chisq.test
assocstats(tab17)        #####calcul du V cramer
sqrt(tab17)

graphrep<-barplot(tab17,main="Etudiants ayant reussi en L1 Economie et Gestion 
par rapport au repéchage au Bac",xlab = "Repêchage",ylab="Reussite en L1",
ylim = c(0,2000), col =c("gray87","chartreuse" ),border=NA)
legend("topright",legend=c("réussite", "echec"),fill = c("chartreuse2","gray87"),bty="n",border=NA)
text(graphrep,c(400,170),c("40.09%","76.94%"),font=2,cex=1)
text(graphrep,c(1200,325),c("59.05%","23.05%"),font=2,cex=1)

###### VQ Mention reussite##### 

mention<-factor(DF$Mention,levels = c("Non","Oui"),labels = c("Sans mention","Avec mention"))
tab18<-table(réussite,mention)                  #### tableau de contingence
tab18
addmargins(tab18)                   #### table de contingence avec les effectifs marginaux
prop.table(margin.table(tab18,1))                #### freq marginale ligne
prop.table(margin.table(tab18,2))
prop.table(tab18,1)                                   #### distributiion conditionnelles ligne
prop.table(tab18,2)                         #### distributions conditionelles colonne
chisq.test((tab18))                              #### Calcul de la statistique Khi
str(chisq.test(tab18))          ###### éléments que calcule la fonction chisq.test
assocstats(tab18)        #####calcul du V cramer
sqrt(tab18)

graphmen<-barplot(tab18,main="Etudiants ayant reussi en L1 Economie et gestion 
selon les résultats obtenus au Bac", xlab = "Résultats du Bac",ylab="Reuissite en L1"
,ylim = c(0,1500),col = c("gray87","chartreuse2"),border=NA)
legend("topright",legend=c("réussite", "echec"),fill = c("chartreuse2","gray87"),bty="n",border=NA)
text(graphmen,c(400,50),c("65.95","13.31%"),font=2,cex=1)
text(graphmen,c(1100,400),c("34.75%","86.68%"),font=2,cex=1)

####### VQ notes math ######

tab19<-table(réussite,DF$Note_math)                  #### tableau de contingence
tab19
addmargins(tab19)                #### table de contingence avec les effectifs marginaux
prop.table(margin.table(tab19,1))                #### freq marginale ligne
prop.table(margin.table(tab19,2))               #### freq marginale collone
prop.table(tab19,1)                                   #### distributiion conditionnelles ligne
prop.table(tab19,2)                         #### distributions conditionelles colonne
chisq.test((tab19))                                        #### Calcul de la statistique Khi
str(chisq.test(tab19))           ###### éléments que calcule la fonction chisq.test

assocstats(tab19)        #####calcul du V cramer
sqrt(tab19)

# Tableau d'analyse de variance 

anova(lm(DF$reussite~DF$Note_math,data = DF))

moy<-tapply(DF$Note_math,réussite,na.rm=TRUE,mean)    ##### comparaison des distribution a partir des variable reussite / notes math
moy
et<-moy<-tapply(DF$Note_math,réussite,na.rm=TRUE,sd)
et
cv<-(et/moy)
cv

quant<-tapply(DF$Note_math,réussite,na.rm=TRUE,quantile,probs=c(0.1,0.25,0.5,0.75,0.9))
quant

#####boxplot##### 

boxplot(DF$Note_math~réussite,main="Distribution des notes math selon les résultat de reussite en L1", xlab="Résultat au Bac",ylab="Notes math", col="antiquewhite" )

abline(h=mean(DF$Note_math,na.rm = TRUE),lty=6,col=2)

points(c(1,2),moy, col=4,lwd=3, pch=16)

##### barplot des effectifs####

graphmath<-barplot(tab19,legend.text=TRUE,ylim = c(0,250),main="Etudiants ayant réussi en L1 
Economie et Gestion selon les notes en math obtenues", xlab="Notes en Math", 
ylab="Réussite en L1",col=rainbow(2), border=NA)

text(graphmath,c(3,20,20,30,60,50,50,50,50,40,30,30,10,10,4,5,3,3),
c("75%","87.87%","86.44%","87,14%","80,04%","73,77%","62.32%","62.47%",
"52.85%","44.50%","41.17%","26.89%","21.71%","20%","10.30%","4.34%","9.61%","9.30%","5.26%"),
font=2,cex=0.75)

text(graphmath,c(10,35,55,65,130,120,120,120,150,140,130,110,110,60,35,35,23,25,22),
c("25%","12.12%","13.55%","12.85%","19.56%","26.27%","37.67%","37.57%","47.14%","55.49%",
"58.82%","73.10%","78.28%","80%","89.69%","95.65%","90.38%","90.69","94.73%"),font=2,cex=0.75)

# Rapport de corrélation

n<-length(réussite)
var_moy<-anova(lm(DF$Note_math~DF$Note_math,na.rm=TRUE,data=DF))[1,2]/n
var_moy
moy_var<-anova(lm(DF$Note_math~réussite,na.rm=TRUE,data=DF))[2,2]/n
moy_var
rap_cor<-sqrt(var_moy/(var_moy+moy_var))
rap_cor

