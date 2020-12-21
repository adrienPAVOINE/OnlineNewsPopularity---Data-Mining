#packages
library(corrplot)
library(ggplot2)
library(plotly)
#import de données
data_all <-read.csv("C:/GITHUB/Data-Mining-Project/OnlineNewsPopularity/OnlineNewsPopularity.csv",sep=",",dec=".")


###########################
#traitements de données   ####################################################################
###########################

#Suppression d'une ligne avec un ratio de mots unique à 700 + suppression url et timedelta
data <- data_all[-31038,-c(1:2)]
summary(data)


#Creation d'une variable popularity si >moyenne alors high sinon low

data$popularity <- "NA"

#Unpopular
#Moderatly popular
#Quite popular
#Very popular

data$popularity[data$shares < 946.1] <- "Unpopular"
data$popularity[(data$shares > 946.1 & data$shares < 1400.1) ] <- "Moderatly popular"
data$popularity[(data$shares > 1400.1 & data$shares < 2800.1)] <- "Quite popular"
data$popularity[data$shares > 2800.1] <- "Very popular"

data$popularity[data$shares < 1400.1] <- "Low"

data$popularity[data$shares > 1400.1] <- "High"

#inversion dummy data pour avoir une variable représentant le jour de la semaine

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

data$count=1
data$day_of_week<-MultChoiceCondense(c("weekday_is_monday","weekday_is_tuesday","weekday_is_wednesday","weekday_is_thursday","weekday_is_friday","weekday_is_saturday","weekday_is_sunday"),data)
data$day_of_week[data$day_of_week == 1] <- "Monday"
data$day_of_week[data$day_of_week == 2] <- "Tuesday"
data$day_of_week[data$day_of_week == 3] <- "Wednesday"
data$day_of_week[data$day_of_week == 4] <- "Thursday"
data$day_of_week[data$day_of_week == 5] <- "Friday"
data$day_of_week[data$day_of_week == 6] <- "Saturday"
data$day_of_week[data$day_of_week == 7] <- "Sunday"

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
#39644 articles dans la base

#en moyenne 3395 partages par article mais il existe des articles très peu ou très publiés
summary(data$shares)

correlation<- as.data.frame(cor(data[,-c(60:63)]))

# avec un histogramme on se rend compte que 38 000 articles sont dans la premières classes
# donc on a à peine 2000 articles qui ont plus de 30 000 partages
# même en bougeant les classes on constate le même résultat

a <- ggplot(data, aes(x = shares))

g <- a + geom_histogram(bins = 30, color = "black", fill = "gray") + geom_vline(aes(xintercept = median(shares)), linetype = "dashed", size = 0.6)

ggplotly(g)

#On va concentrer l'analyse sur les articles de - de 30 000 partages pour regarder plus en détails la répartition

data_inf_30k <- data[which(data$shares<30000), ]


# on obtient des résultats plus clair, presque la moitié des articles ont entre 1000 et 2000 partages
a <- ggplot(data_inf_30k, aes(x = shares))

g <- a + geom_histogram(bins = 5, color = "black", fill = "gray") + geom_vline(aes(xintercept = median(shares)), linetype = "dashed", size = 0.6)

ggplotly(g)

#bon après tout ça on pouvait le savoir grâce au summary (en regardant les quartils)

#a priori pas de grosses corrélation entre une variable et le nombre de partages
print(correlation['shares'])


###########################
#statistiques sur les token ####################################################################
###########################

#mauvaise idée car si les graphs sont plus lisibles on perd l'info sur le nb de partage et de mots
#data_cr <- as.data.frame(scale(data[,3:61],center=T,scale=T))

#répartition normale, le nombre optimal de mots semble centré autour de 10-11
plot(x=data$n_tokens_title,y=data$shares)

#il semblerait que les articles courts soient plus partagés
plot(x=data$n_tokens_content,y=data$shares)

#pas vraiment d'informations à voir
plot(x=data$n_unique_tokens,y=data$shares)

#a priori pas de grosse corrélation
corrplot(cor(data[,c(1:3,59)]))


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

ggplot(data=data, aes(x=theme, y=count)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

#bcp de sans thèmes, mais sinon plus d'articles world et tech

ggplot(data=data, aes(x=theme, y=shares)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

# les articles les plus partagés sont sans thèmes (a virer)


ggplot(data=data, aes(x=theme, y=count, fill=popularity)) +
  geom_bar(stat="identity",position = "fill")

# a contrario les articles sur les thèmes les moins nombreux sont les plus populaires (sans doute car plus originaux)

#########################################
#Préparation des echantillions          ####################################################################
#########################################



#garder que les données quanti
data_qt <- data_inf_30k[,-c(61:63)]

# 75% of the sample size
smp_size <- floor(0.75 * nrow(data_inf_30k))

# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(data_inf_30k)), size = smp_size)

train <- data_qt[train_ind, ]
test <- data_qt[-train_ind, ]

y_train <- as.matrix(train[,60])
y_test <- as.matrix(test[,60])
x_train <- as.matrix(as.matrix(scale(train[,-60],center = T)))
x_test <- as.matrix(as.matrix(scale(test[,-60],center = T)))

#########################################
#Modèle de régression pénalisé          ####################################################################
#########################################

#Install Package
install.packages("glmnet")

#Load Library
library(glmnet)

reg <- glmnet(x_train,y_train,family="binomial",standardize=FALSE,lambda=0)

# Display regression coefficients
coef(reg)

# prediction sur l'échantillon 

pred<- predict(reg,x_test,type="class")

print(as.data.frame(pred)[1])


#c'est nul mdr
plot(y_test)
points(pred,col = "red", pch=16)


# Model performance metrics
install.packages("caret")
library(caret)

data.frame(
  RMSE = RMSE(pred, y_test),
  Rsquare = R2(pred, y_test)
)

#rien de fou, on va faire une selection de variable

# selection de variables vraiment pas folle
mod <- lm(shares~.,data=data_qt)
step(mod, data=data_qt,direction="backward")

#########################################
#Support Vector Machine                 ####################################################################
#########################################

#Install Package
install.packages("e1071")

#Load Library
library(e1071)


#test d'un SVM basique

#Regression with SVM
modelsvm = svm(shares~.,train)

#Predict using SVM regression
predYsvm = predict(modelsvm, data)

#Overlay SVM Predictions on Scatter Plot
points(data$X, predYsvm, col = "red", pch=16)
