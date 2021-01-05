
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

#data_all <-read.csv("C:/GITHUB/Data-Mining-Project/OnlineNewsPopularity/OnlineNewsPopularity.csv",sep=",",dec=".")
data_all <-read.csv("/Volumes/KINGSTON/M2/JAP/Projet_JAP/git/Data-Mining-Project/OnlineNewsPopularity.csv",sep=",",dec=".")



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
confusionMatrix(factor(pred),factor(y_test))

confusionMatrix(factor(pred2),factor(y_test))

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
confusionMatrix(factor(pred),factor(y_test))

confusionMatrix(factor(pred2),factor(y_test))
#0.5125 contre 0.5101

#Les meilleurs modèles sont ceux sans sélections de variables et avec le lambda_min



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
