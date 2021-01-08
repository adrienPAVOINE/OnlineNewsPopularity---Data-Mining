
###########################
#      packages           ####################################################################
###########################

#Install Package
install.packages("glmnet")
# Model performance metrics
install.packages("caret")


#packages
library(corrplot)
library(ggplot2)
library(plotly)
library(caret)
library(glmnet)

###########################
#  import des données     ####################################################################
###########################

data_all <-read.csv("C:/GITHUB/Data-Mining-Project/OnlineNewsPopularity/OnlineNewsPopularity.csv",sep=",",dec=".")
#data_all <-read.csv("/Volumes/KINGSTON/M2/JAP/Projet_JAP/git/Data-Mining-Project/OnlineNewsPopularity.csv",sep=",",dec=".")



###########################
#traitements de données   ####################################################################
###########################

# on regarde s'il y a des NA
sum(is.na(data))

#Suppression d'une ligne avec un ratio de mots unique à 700 
#Suppression des colonnes suivantes :
#   url -> non prédictive
#   timedelta -> non prédictive
#   les 5 variables "LDA"
#   is_weekend -> peut être définie avec les jours de la semaines (is_monday...)
#   kw_min_min, kw_avg_min, kw_min_avg -> ont des valeurs négatives

data <- data_all[-31038,-c(1:2,20,22,26,39:44)]


#on regarde le résumé des données
summary(data)



#Creation d'une variable popularity

data$popularity <- ""

#Unpopular [0,Q1]
#Moderatly popular ]Q1,Q3]
#Very popular ]Q3,+inf[

data$popularity[data$shares < 946.1] <- "Not very Popular"
data$popularity[(data$shares > 946.1 & data$shares < 2800.1) ] <- "Moderatly popular"
data$popularity[data$shares > 2800.1] <- "Very popular"



#inversion dummy data pour avoir une variable représentant le jour de la semaine et une représentant le thème

MultChoiceCondense<-function(vars,indata){
  tempvar<-matrix(NaN,ncol=1,nrow=length(indata[,1]))
  dat<-indata[,vars]
  for (i in 1:length(vars)){
    for (j in 1:length(indata[,1])){
      if (dat[j,i]==1) tempvar[j]=i
    }
  }
  return(tempvar)
}

#creation d'une variable count (pour les graphiques)
data$count=1

#création d'une variable représentant le jour de la semaine
data$day_of_week<-MultChoiceCondense(c("weekday_is_monday","weekday_is_tuesday","weekday_is_wednesday","weekday_is_thursday","weekday_is_friday","weekday_is_saturday","weekday_is_sunday"),data)
data$day_of_week[data$day_of_week == 1] <- "Monday"
data$day_of_week[data$day_of_week == 2] <- "Tuesday"
data$day_of_week[data$day_of_week == 3] <- "Wednesday"
data$day_of_week[data$day_of_week == 4] <- "Thursday"
data$day_of_week[data$day_of_week == 5] <- "Friday"
data$day_of_week[data$day_of_week == 6] <- "Saturday"
data$day_of_week[data$day_of_week == 7] <- "Sunday"

#création d'une variable représentant le thème de l'article
data$theme<-MultChoiceCondense(c("data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world"),data)
data$theme[data$theme == 1] <- "lifestyle"
data$theme[data$theme == 2] <- "entertainment"
data$theme[data$theme == 3] <- "bus"
data$theme[data$theme == 4] <- "socmed"
data$theme[data$theme == 5] <- "tech"
data$theme[data$theme == 6] <- "world"



###########################
#1ere approche globale    ####################################################################
###########################

# Il y a 39644 articles dans la base


summary(data$shares)
#en moyenne 3395 partages par article mais il existe des articles très peu ou très publiés

correlation<- as.data.frame(cor(data[,-c(51:54)]))
print(correlation['shares'])
#a priori pas de grosses corrélation entre une variable et le nombre de partages



a <- ggplot(data, aes(x = shares))

g <- a + geom_histogram(bins = 30, color = "black", fill = "gray") + geom_vline(aes(xintercept = median(shares)), linetype = "dashed", size = 0.6)

ggplotly(g)
# avec un histogramme on se rend compte que 38 000 articles sont dans la première classe
# donc on a à peine 2000 articles qui ont plus de 30 000 partages
# même en bougeant le nombre de classes on constate le même résultat



#On va concentrer l'analyse sur les articles de - de 30 000 partages pour regarder plus en détails la répartition

data_inf_30k <- data[which(data$shares<30000), ]


a <- ggplot(data_inf_30k, aes(x = shares))

g <- a + geom_histogram(bins = 5, color = "black", fill = "gray") + geom_vline(aes(xintercept = median(shares)), linetype = "dashed", size = 0.6)

ggplotly(g)
# on obtient des résultats plus clair, presque la moitié des articles ont entre 1000 et 2000 partages
# bon après tout ça on pouvait le savoir grâce au summary (en regardant les quartils)




###########################
#statistiques sur les token ####################################################################
###########################

plot(x=data$n_tokens_title,y=data$shares)

#répartition normale, le nombre optimal de mots semble centré autour de 10-11

plot(x=data$n_tokens_content,y=data$shares)
#il semblerait que les articles courts soient plus partagés

plot(x=data$n_unique_tokens,y=data$shares)
#pas vraiment d'informations à voir


corrplot(cor(data[,c(1:3,50)]))
#a priori pas de grosse corrélation


#########################################
#statistiques sur les jours de la semaine ####################################################################
#########################################



g<-ggplot(data=data, aes(x=day_of_week, y=count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

g

g<-ggplot(data=data, aes(x=day_of_week, y=shares)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

g

ggplot(data=data, aes(x=day_of_week, y=count, fill=popularity)) +
  geom_bar(stat="identity",position = "fill")

#bcp moins d'articles publiés le wkd mais bcp plus de partages le wkd


#########################################
#statistiques sur les thématiques       ####################################################################
#########################################



ggplot(data=data[data$theme != NaN,], aes(x=theme, y=count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()


ggplot(data=data[data$theme != NaN,], aes(x=theme, y=shares)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()


ggplot(data=data[data$theme != NaN,], aes(x=theme, y=count, fill=popularity)) +
  geom_bar(stat="identity",position = "fill")

#les articles sur les thèmes les moins nombreux sont les plus populaires (sans doute car plus originaux)



#########################################
#Préparation des echantillons          ####################################################################
#########################################



#on ne garde que les données quantitative + la variable prédictive
data_qt <- data[,-c(50,52:54)]



# découpage | 75% train et 25% test
smp_size <- floor(0.75 * nrow(data))

# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data_qt[train_ind, ]

test <- data_qt[-train_ind, ]

#on créer un deuxième dataset avec une selection de features "stepwise"
fit_full <- lm(shares ~ ., data = data[,-c(51:54)])
select_fit<-step(fit_full, k=log(nrow(train)), direction="both")
data_qt2 <- data[,c(labels(select_fit$coefficients)[-1],"popularity")]

# création des x/y train/test
# on centre et on réduit les données pour mettre toutes les données quantitatives à la même échelle
y_train <- as.matrix(train[,50])
y_test <- as.matrix(test[,50])
x_train <- as.matrix(as.matrix(scale(train[,-50],center = T)))
x_test <- as.matrix(as.matrix(scale(test[,-50],center = T)))

#########################################
#Modèle de classification pénalisé      ####################################################################
#########################################

# modèle ELASTICNET

# on effectue une cross validation sur un glmnet pour trouver la valeur optimale du paramètre lambda
cv.reg <- cv.glmnet(x_train,y_train,family="multinomial",alpha=0.5,standardize=FALSE,type.multinomial = "grouped")


plot(cv.reg)
#le graphique montre la cross-validation selon la valeur de lambda. La ligne verticale gauche indique que le log de la valeur optimale du lambda se trouve aux alentours de 6.5, c'est la valeur qui minimise l'erreur

#meilleure valeur de lambda
cv.reg$lambda.min

#création de deux modèles : un avec le lambda_min et un avec le lambda_1se
reg <- glmnet(x_train,y_train,family="multinomial",alpha=0.5,standardize=FALSE,type.multinomial = "grouped",lambda = cv.reg$lambda.min)
reg2 <- glmnet(x_train,y_train,family="multinomial",alpha=0.5,standardize=FALSE,type.multinomial = "grouped",lambda = cv.reg$lambda.1se)



# prediction sur l'échantillon 

pred<- predict(reg,x_test,type="class")
pred2 <-predict(reg2,x_test,type="class")

#resultats
conf<-confusionMatrix(factor(pred),factor(y_test))
conf
conf2<-confusionMatrix(factor(pred2),factor(y_test))
conf2
#0.5167 contre 0.5127 -> le modèle avec le lambda_min est meilleur

#on va essayer avec la selection de variables

train <- data_qt2[train_ind, ]

test <- data_qt2[-train_ind, ]

y_train <- as.matrix(train[,11])
y_test <- as.matrix(test[,11])
x_train <- as.matrix(as.matrix(scale(train[,-11],center = T)))
x_test <- as.matrix(as.matrix(scale(test[,-11],center = T)))


# on effectue une cross validation sur un glmnet pour trouver la valeur optimale du paramètre lambda
cv.reg <- cv.glmnet(x_train,y_train,family="multinomial",alpha=0.5,standardize=FALSE,type.multinomial = "grouped")


plot(cv.reg)
#le graphique montre la cross-validation selon la valeur de lambda. La ligne verticale gauche indique que le log de la valeur optimale du lambda se trouve aux alentours de 6.5, c'est la valeur qui minimise l'erreur

#meilleure valeur de lambda
cv.reg$lambda.min

#création de deux modèles : un avec le lambda_min et un avec le lambda_1se
reg <- glmnet(x_train,y_train,family="multinomial",alpha=0.5,standardize=FALSE,type.multinomial = "grouped",lambda = cv.reg$lambda.min)
reg2 <- glmnet(x_train,y_train,family="multinomial",alpha=0.5,standardize=FALSE,type.multinomial = "grouped",lambda = cv.reg$lambda.1se)



# prediction sur l'échantillon 

pred3<- predict(reg,x_test,type="class")
pred4 <-predict(reg2,x_test,type="class")

#resultats
conf3<-confusionMatrix(factor(pred3),factor(y_test))
conf3

conf4<-confusionMatrix(factor(pred4),factor(y_test))
conf4
#0.5125 contre 0.5101

#Les meilleurs modèles sont ceux sans sélections de variables et avec le lambda_min


#Graphique 

#NFS -> no feature selection
#FS -> feature selection
#lmin -> lambda= lambda_min
#l1se-> lambda= lambda_1se

MEP_df_comp <- function(df){
  #lcol<- colnames(df)
  #lrow <- rownames(df)
  #nrow = nb modele *3 classes
  df_comp<-matrix(data = NA, nrow = 12, ncol = 3)
  df_comp<-as.data.frame(df_comp)
  df_comp[,1]<-c(rep("NFS_lmin",3),rep("NFS_l1se",3),rep("FS_lmin",3),rep("FS_l1se",3))
  df_comp[,2]<-rep(c("Class: Moderatly popular","Class: Not very Popular","Class: Very popular"),4)
  for(i in 1:nrow(df_comp)){
    df_comp[i,3]<- df[df_comp[i,1],df_comp[i,2]]
  }
  df_comp[,3]<- as.numeric(df_comp[,3])
  colnames(df_comp)<- c("model","class","value")
  #df_comp<- as.data.frame(df_comp)
  # df_comp<-as.data.frame(cbind(df_comp[,1],df_comp[,2],df_comp[,3]))
  # df_comp[,"value"]<- as.numeric(df_comp[,"value"])
  return (df_comp)
}

Precision_comp<-as.data.frame(rbind(conf$byClass[,5],conf2$byClass[,5],conf3$byClass[,5],conf4$byClass[,5]))
rownames(Precision_comp)<- c("NFS_lmin","NFS_l1se","FS_lmin","FS_l1se")
Precision_comp <- MEP_df_comp(Precision_comp)
Precision_comp


Recall_comp <- as.data.frame(rbind(conf$byClass[,6],conf2$byClass[,6],conf3$byClass[,6],conf4$byClass[,6]))
rownames(Recall_comp)<- c("NFS_lmin","NFS_l1se","FS_lmin","FS_l1se")
Recall_comp <- MEP_df_comp(Recall_comp)
Recall_comp

Accuracy_comp<-as.data.frame(rbind(conf$overall[1],conf2$overall[1],conf3$overall[1],conf4$overall[1]))
Accuracy_comp$model<- c("NFS_lmin","NFS_l1se","FS_lmin","FS_l1se")
Accuracy_comp

par(mfrow=c(1,1))

graph_pre <- ggplot(data=Precision_comp, aes(x=model, y=value, fill=class)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+ labs(title="Precision comparison")+
  theme_minimal()

graph_rec <- ggplot(data=Recall_comp, aes(x=model, y=value, fill=class)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+ labs(title="Recall comparison")+
  theme_minimal()

graph_acc <- ggplot(data=Accuracy_comp, aes(x=model, y=Accuracy)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+ labs(title="Accuracy comparison")+ theme_minimal()

graph_acc
graph_pre
graph_rec



#########################################
#Support Vector Machine                 ####################################################################
#########################################

#Install Package
install.packages("e1071")

#Load Library
library(e1071)

#test d'un SVM basique

#avant de lancer le svm il faut absolument rÃ©duire le nombre de features sinon le temps d'exÃ©cution va etre abominable
#temps avec selection de features 2-5 minutes
#temps modÃ¨le complet 15-20 minutes
#on va donc reprendre la selection de variable rÃ©alisÃ© prÃ©cedement (data_qt2)

####################
#donnÃ©es data_qt2
####################

smp_size <- floor(0.75 * nrow(data_qt2))
# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(data_qt2)), size = smp_size)
ntrain <- data_qt2[train_ind, ]
ntest <- data_qt2[-train_ind, ]
#centrage reduction des variables explicatives
ntrain[-11] <- scale(ntrain[-11],center=T)
ntrain[,11] <- as.factor(ntrain[,11])
ntest[-11] <- scale(ntest[-11],center=T)
ntest[,11] <- as.factor(ntest[,11])

#rÃ©alisation du modele sur les donnÃ©es avec aucun paramÃ¨tre pour le moment
svmfit_dataqt2 =svm(popularity~., data=ntrain, scale = FALSE)
#accuracy de 0.514
print(svmfit_dataqt2)
#on constate que de base, le kernel est "radial", que le type est c-classification et le cost de 1
#il y a de plus 26836 points supports
#rÃ©alisation de la prÃ©diction
pred = predict(svmfit_dataqt2, ntest[-11], decision.values = TRUE)
table(pred, ntest[,11])
#resultats : accuracy de 0.514
confbase <- confusionMatrix(factor(pred),factor(ntest[,11])) 
#on constate que beaucoup de prÃ©dictions sont "modertly popular". TrÃ¨s peu de Very popular sont prÃ©dits Not very popular


#On souhaite essayer d'autres type de modele avec des kernel diffÃ©rent

svmfitlinear =svm(popularity~., data=ntrain, kernel ="linear", type="C-classification",cost=4)
#ici, absolument tout est prÃ©dit en moyen, mais on obtient quand mÃªme un accuracy de 0.5066
predlinear = predict(svmfitlinear, ntest[-11])
#resultats : accuracy de 0.514
conflinear <- confusionMatrix(factor(predlinear),factor(ntest[,11])) 



svmfitpoly =svm(popularity~., data=ntrain, kernel ="polynomial", degree = 3)
#accuracy de 0.5077
tune_out <- tune.svm(x = ntrain[, -11], y = ntrain[, 11], 
                     type = "C-classification", 
                     kernel = "polynomial", degree = 2, cost = 10^(-1:2), 
                     gamma = c(0.1, 1, 10), coef0 = c(0.1, 1, 10))


svmfitsigmo =svm(popularity~., data=ntrain, kernel ="sigmoid")
#â€¢accuracy de 0.421


#pour chaque modele on rÃ©alise la prÃ©diction suivante
predply = predict(svmfitpoly, ntest[-11], decision.values = TRUE)
confpoly <- confusionMatrix(factor(predply),factor(ntest[,11]))


######tunage des paramÃ¨tres#####
#tunage de gamma et cost####
#trop long Ã  executer 
#svm_tune <- tune(svm, train.x=ntrain[-11], train.y=ntrain[,11], kernel="radial", ranges=list(cost=10^(-2:2), gamma=2^(-2:2)))

#on va donc essayer de tuner avec uniquement un esemble de donnÃ©es, on ne prend que les 2000 premiers individus de train

svm_tune <- tune(svm, train.x=ntrain[1:5000,-11], train.y=ntrain[1:5000,11],kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,10,2)))
print(svm_tune)
plot(svm_tune)
plot(svm_tune, transform.x = log2, transform.y = log2)

#on constate que le meilleur cost est 0.1 et le meilleur gamma est 0.5.On relance recalcule alors le modele avec ces paramÃ¨tres.
svmfitaftertune =svm(popularity~., data=ntrain, kernel ="radial", type="C-classification",gamma =0.5,cost =0.1)
#pour chaque modele on rÃ©alise la prÃ©diction suivante
pred_tune = predict(svmfitaftertune, ntest[-11], decision.values = TRUE)
table(pred_tune, ntest[,11])
#resultats
conftune <- confusionMatrix(factor(pred_tune),factor(ntest[,11]))


#####test en ne gardant que Very popular####

#On souhaite maintenant rÃ©aliser un SVM en nous basant sur le fait d'Ãªtre trÃ¨s populaire ou non
pop <- as.factor(ntrain$popularity == "Very popular")
ntrain2 = scale(ntrain[,-11])
# fit binary C-classification model
m <- svm(pop ~ .,data = ntrain2, kernel = "radial")
#rÃ©alisation de la prÃ©diction
pred = predict(m, ntest[-11])
table(pred, ntest[,11])


#########################################
#RÃ©duction avec une ACP
#########################################
#On essaye de rÃ©duire les dimensions pour ne retenir que 2 axes (surtout pour essayer une visualisation graphique)
library("FactoMineR")
library("factoextra")

res.pca <- PCA(data_qt[-50], scale.unit = TRUE, ncp = 2, graph = TRUE)
ind <- get_pca_ind(res.pca)
daf <- data.frame(cbind(ind$coord, data_qt$popularity))
daf[,1] <- round(as.double(daf[,1]),3)
daf[,2] <- round(as.double(daf[,2]),3)
plot(daf$Dim.1,daf$Dim.2)
# 75% of the sample size
smp_size <- floor(0.75 * nrow(daf))
# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(daf)), size = smp_size)
ftrain <- daf[train_ind, ]
ftest <- daf[-train_ind, ]

ftrain[,3] <- as.factor(ftrain[,3])
ftest[,3] <- as.factor(ftest[,3])
#on rÃ©alise le modele sur ces doonnÃ©es
svmfit =svm(V3~., data=ftrain, kernel ="radial", type="C-classification",gamma =0.5,cost =1)
#avec 2 dimensions, on peut afficher un graphique 
plot(svmfit, ftrain, Dim.1 ~ Dim.2,
     slice=list(Dim.1=3, Dim.2=4))

svp <- ksvm(V3~.,data =ftrain,type="C-svc",kernel='rbf',kpar=list(sigma=1),C=1)


plot(ftrain$Dim.1,ftrain$Dim.2,type="n")
text(ftrain$Dim.1,ftrain$Dim.2,rownames(ftrain),col=c("blue","red")[ftrain$V3],cex=0.75)
points(ftrain$Dim.1[svp$index],ftrain$Dim.2[svp$index],cex=3,col=rgb(0,0,0))

beta.0 <- -svp$rho
print(beta.0)
#coefficients
beta.1 <- sum(svp$coefs*ftrain$Dim.1[svp$index])
beta.2 <- sum(svp$coefs*ftrain$Dim.2[svp$index])
print(paste(beta.1,beta.2))
abline(-beta.0/beta.2,-beta.1/beta.2,col="green")
#the frontiÃ¨res Â« marginales Â»
abline((-beta.0-1.0)/beta.2,-beta.1/beta.2,col="gray")
abline((-beta.0+1.0)/beta.2,-beta.1/beta.2,col="gray")


#rÃ©alisation de la prÃ©diction
pred = predict(svp, ftest[-3])
table(pred, ftest[,3])
#on constate que presque tout est prÃ©dit populaire
#resultats
confusionMatrix(factor(pred),factor(ftest[,3])) 
#on obtient un taux d'accuracy de 0.5062

##############5 axes#################################

#RÃ©alise le mÃªme test avec toutes les donnÃ©es quanti et en gardant 5 axes 
res.pca <- PCA(data_qt[-50], ncp=5,scale.unit = TRUE, graph = TRUE)
ind <- get_pca_ind(res.pca)

daf <- data.frame(cbind(ind$coord, data_qt$popularity))
for (j in 1:ncol(ind$coord)){
  daf[,j] <- round(as.double(daf[,j]),3)
}


# 75% of the sample size
smp_size <- floor(0.75 * nrow(daf))
# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(daf)), size = smp_size)
ftrain <- daf[train_ind, ]
ftest <- daf[-train_ind, ]

ftrain[,6] <- as.factor(ftrain[,6])
ftest[,6] <- as.factor(ftest[,6])
#on rÃ©alise le modele sur ces doonnÃ©es
svmfitACP5 =svm(V6~., data=ftrain, kernel ="radial", type="C-classification",gamma =0.1,cost =10)

#rÃ©alisation de la prÃ©diction
predACP5 = predict(svmfitACP5, ftest[-6])
table(predACP5, ftest[,6])
#on constate que presque tout est prÃ©dit populaire mais un peu mieux que prÃ©cedant
#resultats
confusionMatrix(factor(predACP5),factor(ftest[,6])) 
#on obtient un taux d'accuracy de 0.5077

#################22 axes###############################â˜»

#RÃ©alise le mÃªme test avec toutes les donnÃ©es quanti et en gardant 22 axes (car on doit en garder 22 pour avoir une inertie de plus de 80%)
res.pca <- PCA(data_qt[-50], ncp=22,scale.unit = TRUE, graph = TRUE)
ind <- get_pca_ind(res.pca)

daf <- data.frame(cbind(ind$coord, data_qt$popularity))
for (j in 1:ncol(ind$coord)){
  daf[,j] <- round(as.double(daf[,j]),3)
}

# 75% of the sample size
smp_size <- floor(0.75 * nrow(daf))
# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(daf)), size = smp_size)
ftrain <- daf[train_ind, ]
ftest <- daf[-train_ind, ]

ftrain[,23] <- as.factor(ftrain[,23])
ftest[,23] <- as.factor(ftest[,23])
#on rÃ©alise le modele sur ces doonnÃ©es

svm_tune <- tune(svm, train.x=ftrain[1:1000,-23], train.y=ftrain[1:1000,23],kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

svmfitACP22 =svm(V23~., data=ftrain, kernel ="radial", type="C-classification",gamma =1,cost =1)

#rÃ©alisation de la prÃ©diction
predACP22 = predict(svmfitACP22, ftest[-23])
table(predACP22, ftest[,23])
#on constate que presque tout est prÃ©dit populaire mais un peu mieux que prÃ©cedant
#resultats
confACP22 <- confusionMatrix(factor(predACP22),factor(ftest[,23])) 
#on obtient un taux d'accuracy de 0.4867


######################################
#donnÃ©es toutes les donnÃ©es quanti
######################################
#centrage reduction des variables explicatives

train[-50] <- scale(train[-50],center=T)
train[,50] <- as.factor(train[,50])
test[-50] <- scale(test[-50],center=T)
test[,50] <- as.factor(test[,50])
#avec 50 variables, le temps d'exÃ©cution est considÃ©rable, on essaye donc de rÃ©aliser un svm sur un ensemble de donnÃ©es
#on ne prend donc que 1000 individus
svm_tune <- tune(svm, train.x=train[1:3000,-50], train.y=train[1:3000,50],kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

#Regression with SVM
svmTot = svm(popularity~.,data = train, kernel="radial",type="C-classification", cost = 1, gamma=1)

#Predict using SVM regression
predYsvmTot = predict(svmTot, test[-50])

#resultats
conftot <- confusionMatrix(factor(predYsvmTot),factor(test[,50])) 

####On constate ainsi que la classe "moderatly popular" est sur reprÃ©sentÃ© dans les prÃ©dictions

#On va essayer de rÃ©aliser un under sampling pour sous reprÃ©senter les "moyen populaires" trop souvent prÃ©dit


######################################
#donnÃ©es under sampling sur data-qt2
######################################
library("ROSE")
datata = subset(ntrain,ntrain$popularity=="Moderatly popular" | ntrain$popularity=="Very popular")
# imbalance on training set
table(ntrain$popularity)
# balanced data set with both over and under sampling
data.balanced.ou <- ovun.sample(popularity~., data=datata, p=0.5, 
                                seed=5, method="under")$data

table(data.balanced.ou$popularity)

notpop <- subset(ntrain,ntrain$popularity=="Not very Popular")
fntrain <- rbind(data.balanced.ou,notpop)

smp_size <- floor(0.75 * nrow())
# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(daf)), size = smp_size)
ftrain <- daf[train_ind, ]

svm_tune <- tune(svm, train.x=fntrain[,-11], train.y=fntrain[,11],kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))


print(svm_tune)
plot(svm_tune)
#on rÃ©alise maintenant le modele sur les nouvelles donnÃ©es
svmfitunder =svm(popularity~., data=fntrain, kernel ="radial", type="C-classification",gamma =1,cost =80)

predunder = predict(svmfitunder, ntest[-11])
table(predunder, ntest[,11])
#on constate que presque tout est prÃ©dit populaire mais un peu mieux que prÃ©cedant
#resultats
confunder <- confusionMatrix(factor(predunder),factor(ntest[,11])) 
#on obtient un taux d'accuracy de 0.42 moins bien 









MEP_df_comp <- function(df){
  #lcol<- colnames(df)
  #lrow <- rownames(df)
  #nrow = nb modele *3 classes
  df_comp<-matrix(data = NA, nrow = 18, ncol = 3)
  df_comp<-as.data.frame(df_comp)
  df_comp[,1]<-c(rep("SVM de base radial",3),rep("SVM linear",3),rep("SVM polynomial ",3),rep("SVM tunÃ© radial",3), rep("SVM avec ACP",3), rep("SVM apres undersampling",3))
  df_comp[,2]<-rep(c("Class: Moderatly popular","Class: Not very Popular","Class: Very popular"),6)
  for(i in 1:nrow(df_comp)){
    df_comp[i,3]<- df[df_comp[i,1],df_comp[i,2]]
  }
  df_comp[,3]<- as.numeric(df_comp[,3])
  colnames(df_comp)<- c("model","class","value")
  #df_comp<- as.data.frame(df_comp)
  # df_comp<-as.data.frame(cbind(df_comp[,1],df_comp[,2],df_comp[,3]))
  # df_comp[,"value"]<- as.numeric(df_comp[,"value"])
  return (df_comp)
}



Precision_comp<-as.data.frame(rbind(confbase$byClass[,5],conflinear$byClass[,5],confpoly$byClass[,5],conftune$byClass[,5], confACP22$byClass[,5],confunder$byClass[,5]))
rownames(Precision_comp)<- c("SVM de base radial","SVM linear","SVM polynomial ","SVM tunÃ© radial", "SVM avec ACP", "SVM apres undersampling")
Precision_comp <- MEP_df_comp(Precision_comp)
Precision_comp


Recall_comp<-as.data.frame(rbind(confbase$byClass[,6],conflinear$byClass[,6],confpoly$byClass[,6],conftune$byClass[,6], confACP22$byClass[,6],confunder$byClass[,6]))
rownames(Recall_comp)<- c("SVM de base radial","SVM linear","SVM polynomial ","SVM tunÃ© radial", "SVM avec ACP", "SVM apres undersampling")
Recall_comp <- MEP_df_comp(Recall_comp)
Recall_comp

Accuracy_comp<-as.data.frame(rbind(confbase$overall[1],conflinear$overall[1],confpoly$overall[1],conftune$overall[1], confACP22$overall[1],confunder$overall[1]))
Accuracy_comp$model<- c("SVM de base radial","SVM linear","SVM polynomial ","SVM tunÃ© radial", "SVM avec ACP", "SVM apres undersampling")
Accuracy_comp



par(mfrow=c(1,1))

graph_pre <- ggplot(data=Precision_comp, aes(x=model, y=value, fill=class)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+ labs(title="Precision comparison")+
  theme_minimal()

graph_rec <- ggplot(data=Recall_comp, aes(x=model, y=value, fill=class)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+ labs(title="Recall comparison")+
  theme_minimal()

graph_acc <- ggplot(data=Accuracy_comp, aes(x=model, y=Accuracy)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+ labs(title="Accuracy comparison")+ theme_minimal()

graph_acc
graph_pre
graph_rec


###############################
#      Réseaux de Neurones    ####################################################################
###############################

#---1ere approche : Librairie nnet : Réseau avec une couche cachée

library(nnet)
set.seed(100)

#Récuperation de la variable cible sous fomre de facteurs
popu_fact <- as.factor(train[,'popularity'])

#Réseau avec 1 couche cachee (skip) de 3 neuronnes (size) --> ne converge pas 
#ps.nnet <- nnet(popu_fact ~ ., data = x_train, skip = F, size = 3,na.action = na.omit, maxit=300 )

#Réseau avec 1 couche cachee (skip) de 10 neuronnes (size)
ps.nnet <- nnet(popu_fact ~ ., data = x_train, skip = F, size = 10,na.action = na.omit, maxit=350 )


#---Utilisation de la librairie H2O

library(h2o)

trainnnh2o <- train
#Transformation de la variable cible en type facteur
trainnnh2o$popularity <- as.factor(trainnnh2o$popularity )

#Initialisation : nthreads = -1, utiliser tous les coeurs disponibles
#h2o.init(nthreads = -1)
h2o.init(nthreads = 1)

#Transformation format reconnu par h2o
h2oTrain <- as.h2o(trainnnh2o)

#Memes manipulations pour l'echantillon test 
testnnh2o <- test
testnnh2o$popularity <- as.factor(testnnh2o$popularity )

h2oTest <- as.h2o(testnnh2o)

#Modélisation : 1 couche cachee avec 3 neuronnes
# - Validation croisee avec nfolds 
# - On garde les prédictions de la validation croisee : keep_cross_validation_predictions
# - Fonction d'activation similaire à sigmoid : Tanh
pm.h2o <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,activation="Tanh",seed=100,hidden=c(3),nfolds=5,
                           keep_cross_validation_predictions=T)

#Prédiction
pred.pm.h20 <- h2o.predict(pm.h2o,newdata=h2oTest)

#Matrice de confusion
h2o.confusionMatrix(pm.h2o,newdata=h2oTest)

#OU affichage plus complet 
#évaluation
mat_pred<-as.matrix(pred.pm.h20)[,"predict"]
mat_pred<-as.factor(mat_pred)
cm<-confusionMatrix(mat_pred,testnnh2o$popularity)
print(cm)
#Differentes metriques 
print(cm$byClass)


#Accuracy  de 0,5182 entre [0,5083 ; 0,5281]  --> plutôt moyen 

#Beaucoup de prédictions en "Moderatly popular" 


#---Recherche d'hyper parametres aléatoire 

#On veut tester plusieurs fonctions d'activation et faire varier le nombre de neuronnes de la couche cachée 
hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=list(2,3,4,5,6,7,8,9,10,15,20,25,30)
)

#https://docs.h2o.ai/h2o-tutorials/latest-stable/tutorials/deeplearning/index.html
## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 200, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_random",
  training_frame=h2oTrain,
  y="popularity",
  epochs=1,
  stopping_metric="logloss",
  stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  #On utilise la validation croisée
  nfolds=5,
  keep_cross_validation_predictions=T
)       

#Differents modeles testes
grid <- h2o.getGrid("dl_grid_random",sort_by="logloss",decreasing=FALSE)
print(grid)

#On recupere le meilleur modele
best_nn <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
print(best_nn)
#on voit qu'il reste quand même un taux d'erreur assez important pour Not very et very popular (sur le train)
# couche cacchee: 8 neuronnes et Tanh, sortie : Softmax

#Predictions avec le meilleur modele

pred_bestnn <- h2o.predict(best_nn,newdata=h2oTest)
pred_bestnn_mat<-as.matrix(pred_bestnn)[,"predict"]
pred_bestnn_mat<-as.factor(pred_bestnn_mat)
cm_bestnn<-confusionMatrix(pred_bestnn_mat,testnnh2o$popularity)
print(cm_bestnn)
#Differentes metriques 
print(cm_bestnn$byClass)

#Accuracy 0,5108 [0.501, 0.5207]
#Pas mieux que premier essai  MAIS idee de la focntion d'activation --> très peu de not very popular en very et inversement


#On regarde l'importance des variables 

var_imp_bestnn<-as.data.frame(h2o.varimp(best_nn))
print(var_imp_bestnn)


#Tentative de recherche du meilleur modele en selectionnant les variables avec plus d'importance 
# ==> pas d'amelioration significative
#On garde les variables dont l'importance relative est supéerieure a 0,5
#var_best_bestnn<-var_imp_bestnn$variable[var_imp_bestnn$relative_importance>0.5] 
#Selection des variables "importantes" dans les echantillons
#trainnnh2o <- train[,c(var_best_bestnn,'popularity')]
#trainnnh2o$popularity <- as.factor(trainnnh2o$popularity )
#transformer en un format reconnu par h2o
#h2oTrain <- as.h2o(trainnnh2o)
#testnnh2o <- test[,c(var_best_bestnn,'popularity')]
#testnnh2o$popularity <- as.factor(testnnh2o$popularity )
#h2oTest <- as.h2o(testnnh2o)


#---Tentative avec 2 couches

nn_h2o_bis <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,activation="Tanh",seed=100,hidden=c(40,10),nfolds=5,
                               keep_cross_validation_predictions=T) 

#Prédiction
pred_nn_h20_bis <- h2o.predict(nn_h2o_bis,newdata=h2oTest)

#Matrice de confusion

mat_pred_bis<-as.matrix(pred_nn_h20_bis)[,"predict"]
mat_pred_bis<-as.factor(mat_pred_bis)
cmbis<-confusionMatrix(mat_pred_bis,testnnh2o$popularity)
print(cmbis)
#Differentes metriques 
print(cmbis$byClass)


# Intervalle de confiance - Accuracy - Nombre de neuronnes des couches cachees
# (0.5046, 0.5244) 0,5145  30,9
# 0,5078, 0,5276  0,5177  27,9
# (0.5084, 0.5282) 0.5183  36,9
#  (0.511, 0.5308) 0.5209     40,9
#  (0.5114, 0.5312) 0.5213    40,15
#  (0.5152, 0.5349) 0.5214    40,10
#(0.5091, 0.5289)  0;519  40,20
#(0.5108, 0.5306)  0.5207     45,10
# (0.5074, 0.5272) 0.5173  27,18,9
#(0.5075, 0.5273)  0;5774  36,18,9

#Autour de couches cachees a 40 puis 10 couches cachees la precision semble plus haute par rapport aux autres combinaisons


#---Tentative d'oversampling sur les classes "Not very popular" et "Very popular" 
# ==> pas vraiment efficace : abandon 

# library(smotefamily)
# 
# xsmote<-as.data.frame(x_train)
# ysmote<-as.factor(y_train)
# 
# #Oversampling sur la classe la moins representee (Very popular)
# balanced.data <- SMOTE(X=xsmote, target = ysmote ,K =5, dup_size = 1 ) 
# new_data = balanced.data$syn_data
# bdata = balanced.data$data
# table(ysmote)
# table(bdata$class)
# 
# #Oversampling sur la deuxieme classe la moins representee (Not very popular) a partir du premier oversampling
# balanced.data2 <- SMOTE(X=bdata[,1:(ncol(bdata)-1)], target = bdata[,ncol(bdata)] ,K =5, dup_size = 1 ) 
# new_data2 = balanced.data2$syn_data
# bdata2 = balanced.data2$data
# table(ysmote)
# table(bdata$class)
# table(bdata2$class)
# colnames(bdata2)[colnames(bdata2) == "class"] <- "popularity"
# 
# h2oTrain<-bdata2
# h2oTrain$popularity <- as.factor(h2oTrain$popularity)
# h2oTrain <- as.h2o(h2oTrain)
# h2oTrain[,"popularity"] <- as.factor(h2oTrain[,"popularity"])
# 

#Graphique 

#NFS -> no feature selection
#FS -> feature selection
#lmin -> lambda= lambda_min
#l1se-> lambda= lambda_1se

MEP_df_comp <- function(df,nmod){
  df_comp<-matrix(data = NA, nrow = nmod, ncol = 3)
  df_comp<-as.data.frame(df_comp)
  df_comp[,1]<-c(rep("NN_1c_3n",3),rep("NN_1c_8N",3),rep("NN_2c_40+10N",3))
  df_comp[,2]<-rep(c("Class: Moderatly popular","Class: Not very Popular","Class: Very popular"),3)
  for(i in 1:nrow(df_comp)){
    df_comp[i,3]<- df[df_comp[i,1],df_comp[i,2]]
  }
  colnames(df_comp)<- c("model","class","value")
  return (df_comp)
}


Precision_compNN<-as.data.frame(rbind(cm$byClass[,5],cm_bestnn$byClass[,5],cmbis$byClass[,5]))
rownames(Precision_compNN)<- c("NN_1c_3n","NN_1c_8N","NN_2c_40+10N")
Precision_compNN <- MEP_df_comp(Precision_compNN,9)


Recall_compNN <- as.data.frame(rbind(cm$byClass[,6],cm_bestnn$byClass[,6],cmbis$byClass[,6]))
rownames(Recall_compNN)<- c("NN_1c_3n","NN_1c_8N","NN_2c_40+10N")
df<-Recall_compNN
Recall_compNN <- MEP_df_comp(Recall_compNN,9)
Recall_compNN

Accuracy_compNN<-as.data.frame(rbind(cm$overall[1],cm_bestnn$overall[1],cmbis$overall[1]))
Accuracy_compNN$model<- c("NN_1c_3n","NN_1c_8N","NN_2c_40+10N")
Accuracy_compNN

par(mfrow=c(1,1))

graph_pre <- ggplot(data=Precision_compNN, aes(x=model, y=value, fill=class)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+ labs(title="Precision comparison")+
  theme_minimal()

graph_rec <- ggplot(data=Recall_compNN, aes(x=model, y=as.numeric(value), fill=class)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+ labs(title="Recall comparison")+
  theme_minimal()

graph_acc <- ggplot(data=Accuracy_compNN, aes(x=model, y=Accuracy)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+ labs(title="Accuracy comparison")+ theme_minimal()

graph_acc
graph_pre
graph_rec

#arrêt de H2o
h2o.shutdown()

#########################################
#        RANDOM FOREST                  ####################################################################
#########################################

install.packages("randomForest")
library(randomForest)

# Conversion de la popularité en factor
data_qt$popularity<-factor(data_qt$popularity)

# Mise en place d'un randomForest, on a trouvé les paramètres en fonctionnant à tâton
RandomForest_News <- randomForest(popularity~.,data=data_qt, ntree = 200, 
                                  mtry = 3, na.action = na.roughfix)

print(RandomForest_News)

# On cherche en tunnant les paramètres, on ne prend que les 5000 premières observations car sinon c'est beaucoup trop long de tout tuner
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
set.seed(123)
mtry <- 10
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=mtry)
rf_random <- train(popularity~., data=data_qt[1:5000,], method="rf", metric=metric, trControl=control)
print(rf_random)
plot(rf_random) 

#en tunnant on arrive à atteindre les 55% d'accuracy
pred5 <- RandomForest_News$predicted
pred5

conf4<-confusionMatrix(factor(pred5),factor(data_qt[,'popularity']))

conf4
