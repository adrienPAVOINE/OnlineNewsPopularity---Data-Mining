#------------------------ 2. LECTURE ET DESCRIPTION DES DONNEES ------------------------

#---Question 2 : Chargement des donnees---

#Recuperation du dossier dans lequel se toruve le fichier 
direc<-dirname(rstudioapi::getSourceEditorContext()$path)

#Assignation de l'environnement de travail 
setwd(direc)

#Récupération des données 
D <- read.table("breast-cancer-wisconsin.data",sep=",",na.strings = "?")


#---Question 3 : Description et aperçu des données---

#Type de l'objet D
class(D) 

#Nombre d'observation, Nombre de variables, Liste des variables avec leur type + aperçu 
str(D) 

#Affichage des premières lignes du data frame
head(D) 

#Statitiques basiques pour chaque variable 
summary(D)



#------------------------ 3. SEPARATION DES DONNEES EN "TRAIN" ET "TEST" ------------------------

#---Question 4

#Récuperation des observations comportant au moins une donnée manquante 
missval<-which(complete.cases(D)==F)

#Affichage des observations comportant au moins une donnée manquante 
print(missval)

#Nombre d'observations comportant au moins une donnée manquante 
print(length(missval))


#---Question 5 : 

#On en conserve que observations sans données manquantes   
#D<-D[which(complete.cases(D)),]
D<-D[-missval,]


#---Question 6 : 

#Variables explicatives  
X<-D[,c(2:10)]

#Variable cible 
y<-D[,11]


#---Question 7 : Recodage de la variable cible 

library(dplyr)
y<-recode(y, '2' = 0 , '4' = 1)


#---Question 8 : 

#Indices des observations des tumeurs benignes 
benin<-which(y==0)

#Indices des observations des tumeurs malignes
malin<-which(y==1)


#---Question 9 :

#Indices de train =  Indices des 200 premières observations bégnines
train_set <- head(benin,200)

#Ensemble train = 200 premières observations bégnines
Xtrain <- X[train_set,]
ytrain<- y[train_set]

#Indices de test =  Indices des observations bégnines restantes + indices des observations malignes 
test_set <- c(benin[201:444],malin)

#Ensemble test =  Observations bégnines restantes + observations malignes 
Xtest <- X[test_set,]
ytest <-y[test_set]


#------------------------ 4. ONE-CLASS SVM ------------------------

#---Question 10

library(e1071)


#---Question 11 : Estimation du modèle avec l'ensemble train

oc_svm_fit <- svm(Xtrain,ytrain,type='one-classification', gamma=1/2)


#---Question 12 : prédiction des scores pour les observations de test

oc_svm_pred_test <- predict(oc_svm_fit,Xtest,decision.values = TRUE)


#---Question 13 : récupération de l’attribut "decision.values" de oc_svm_score_test. Inversement du signe des scores pour pouvoir ensuite obtenir une courbe ROC plus lisible.

attr(oc_svm_pred_test," decision.values ")
oc_svm_score_test = -as.numeric(attr(oc_svm_pred_test,"decision.values"))


#------------------------ 5. COURBE ROC ------------------------

#---Question 14
library(ROCR)


#---Question 15

#Obtention des prédictions
pred_oc_svm = prediction(oc_svm_score_test, y[test_set])

#confrontation taux de faux positifs VS taux de vrais positifs 
oc_svm_roc = performance(pred_oc_svm , measure = "tpr" , x.measure= "fpr")

#Affichage de la courbe ROC 
plot(oc_svm_roc)

#Question 16

#Calcul de l'AUC
perf_svm<- performance(pred_oc_svm , measure = "auc" )
print(perf_svm@y.values)


#------------------------ 6. KERNEL PCA ------------------------

#---Question 17 

library(kernlab)
kernel=rbfdot(sigma=1/8) 
kernel

mat_trainset<-as.matrix(X[train_set ,])
Ktrain=kernelMatrix(kernel ,x= mat_trainset)

n<- nrow(Ktrain )  #pas sur


#---Question 18 

k2=apply(Ktrain,1,sum) 
k3=apply(Ktrain ,2,sum) 
k4=sum(Ktrain) 
KtrainCent=matrix(0,ncol=n,nrow=n) 
for (i in 1:n)
{
  for (j in 1:n) {
    KtrainCent[i,j]=Ktrain[i,j]-1/n*k2[i]-1/n*k3[j]+1/n^2*k4 
  }
}


#---Question 19 : Decomposition spectrale de KtrainCent

eigen_KtrainCent<-eigen(KtrainCent)


#---Question 20

s<-80
A=eigen_KtrainCent$vectors[,1:s]%*%diag(1/sqrt( eigen_KtrainCent$values[1:s]))


#---Question 21

matX<-as.matrix(X)
K=kernelMatrix(kernel ,matX)


#---Question 22

p1 <-( K )
p2 <-apply(K[,train_set] ,1 , sum )
p3 <- sum(K[train_set,train_set])


#---Question 23

ps<-NULL
#n<-200

i<-1
for (z in test_set )
{
  ps[i]= p1[z,z] -(2/n) * p2[z] + (1/n^2)* p3
  i<-i+1
}


#---Question 24

f1<-K[test_set,train_set]
f2<-p2[train_set]
f3<-p2[test_set]
f4<-p3


#---Question 25

n2<-length(ytest) 
fl<-matrix(0,ncol=s,nrow=n2)

for (m in 1:s){
  i<-0
  for (z in test_set ){
    i<-i+1
    temp<-0
    for (i2 in 1:n){
      
      temp <- temp +(A[i2,m]*( f1[i,i2] - (1/n)*f2[i2] - (1/n)*f3[i] + (1/n^2)*f4))
    }
    fl[i,m]<-temp
  }
}


#---Question 26 

kpca_score_test <- ps - apply(fl^2,1,sum)


#Question 27 : Courbes ROC 

#Courbe ROC Kernel PCA
pred_oc_acp=prediction(kpca_score_test ,ytest)
oc_svm_acp = performance(pred_oc_acp, measure = "tpr", x.measure
                         = "fpr")

#Affichage de la courbe ROC Kernel PCA
plot(oc_svm_acp)

#Affichage des 2 courbes ROC
plot(oc_svm_roc )
plot(oc_svm_acp, add=T,col="red")

#Calcul des AUC 

print(perf_svm@y.values)

perf_acp<-performance(pred_oc_acp, measure = "auc")
print(perf_acp@y.values)
