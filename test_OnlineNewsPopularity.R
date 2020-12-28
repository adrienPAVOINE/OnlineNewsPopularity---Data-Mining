
###########################
#      packages           ####################################################################
###########################

#packages
library(corrplot)
library(ggplot2)
library(plotly)
# Model performance metrics
install.packages("caret")
library(caret)


#Install Package
install.packages("glmnet")

#Load Library
library(glmnet)
#import de données
data_all <-read.csv("C:/GITHUB/Data-Mining-Project/OnlineNewsPopularity/OnlineNewsPopularity.csv",sep=",",dec=".")


###########################
#traitements de données   ####################################################################
###########################

# checking for NaN
sum(is.na(data))

#Suppression d'une ligne avec un ratio de mots unique à 700 + suppression url et timedelta
data <- data_all[-31038,-c(1:2)]

summary(data)



#Creation d'une variable popularity si >moyenne alors high sinon low

data$popularity <- ""

#Unpopular
#Moderatly popular
#Very popular

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

summary(log(data$shares))

0.88/7.437
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
#Préparation des echantillons          ####################################################################
#########################################



#garder que les données quanti
data_qt <- data[,-c(59,61:63)]

#data avec selection de features 
data_qt2 <- data_inf_30k[,c(12:16,18:20,25:26,31:33,35:36,38,43,47,59)]

# 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data_qt[train_ind, ]

test <- data_qt[-train_ind, ]

train[]

y_train <- as.matrix(train[,59])
y_test <- as.matrix(test[,59])
x_train <- as.matrix(as.matrix(scale(train[,-59],center = T)))
x_test <- as.matrix(as.matrix(scale(test[,-59],center = T)))

#########################################
#Modèle de classification pénalisé      ####################################################################
#########################################



# modèle LASSO

reg <- glmnet(x_train,y_train,family="multinomial",alpha=1,standardize=FALSE,type.multinomial = "grouped",lambda=0)

# Display regression coefficients
print(coef(reg))

# prediction sur l'échantillon 

pred<- predict(reg,x_test,type="class")


#resultats
confusionMatrix(factor(pred),factor(y_test))

# modèle RIDGE

reg <- glmnet(x_train,y_train,family="multinomial",alpha=0,standardize=FALSE,type.multinomial = "grouped",lambda=0)

# Display regression coefficients
print(coef(reg))

# prediction sur l'échantillon 

pred<- predict(reg,x_test,type="class")


#resultats
confusionMatrix(factor(pred),factor(y_test))


# modèle ELASTICNET

cv.reg <- cv.glmnet(x_train,y_train,family="multinomial",alpha=0.5,standardize=FALSE,type.multinomial = "grouped")


plot(cv.reg)
#The plot displays the cross-validation error according to the log of lambda. The left dashed vertical line indicates that the log of the optimal value of lambda is approximately -6.5, which is the one that minimizes the prediction error

#best value for lambda
cv.reg$lambda.min

#model
reg <- glmnet(x_train,y_train,family="multinomial",alpha=0.5,standardize=FALSE,type.multinomial = "grouped",lambda = cv.reg$lambda.min)
reg2 <- glmnet(x_train,y_train,family="multinomial",alpha=0.5,standardize=FALSE,type.multinomial = "grouped",lambda = cv.reg$lambda.1se)


# Display regression coefficients
print(coef(reg))

# prediction sur l'échantillon 

pred<- predict(reg,x_test,type="class")
pred2 <-predict(reg2,x_test,type="class")

#resultats
confusionMatrix(factor(pred2),factor(y_test))



#########################################
#Support Vector Machine                 ####################################################################
#########################################

#Install Package
install.packages("e1071")

#Load Library
library(e1071)




#test d'un SVM basique

#centrage reduction des variables explicatives
train[-59] <- scale(train[-59],center=T)
train[,59] <- as.factor(train[,59])
test[-59] <- scale(test[-59],center=T)
test[,59] <- as.factor(test[,59])

#avant de lancer le svm il faut absolument réduire le nombre de features sinon le temps d'exécution va etre abominable
#temps avec selection de features 2-5 minutes
#temps modèle complet 15-20 minutes


#Regression with SVM
#modelsvm = svm(popularity~.,train[,c(12:16,18:20,25:26,31:33,35:36,38,43,47,59)],kernel="radial",type="C-classification")

modelsvm = svm(popularity~.,train,kernel="radial",type="C-classification")

#Predict using SVM regression
predYsvm = predict(modelsvm, test[-59])

#resultats
confusionMatrix(factor(predYsvm),factor(y_test)) 

#########################################
#        NEURAL NETWORK                 ####################################################################
#########################################

#--------------NN Antho-----------------------

#____________LIBRARY NNET______________________

library(nnet)
set.seed(100)

ciblennet <- as.factor(train[,'popularity'])

ps.nnet <- nnet(ciblennet ~ ., data = x_train, skip = F, size = 3,na.action = na.omit, maxit=500 )

#prediction
pred.ps.nnet <- predict(ps.nnet,newdata=test, type="class")

pred.ps.nnet<-as.factor(pred.ps.nnet)
confusionMatrix(as.factor(test$popularity) ,pred.ps.nnet )

#3 : 0,4113 


#____________LIBRARY H2O______________________

library(h2o)

trainnnh2o <- train
trainnnh2o$popularity <- as.factor(trainnnh2o$popularity )

#initialisation - nthreads = -1, utiliser tous les c??urs disponibles
h2o.init(nthreads = -1)

#transformer en un format reconnu par h2o
h2oTrain <- as.h2o(trainnnh2o)

print(head(h2oTrain))

testnnh2o <- test
testnnh2o$popularity <- as.factor(testnnh2o$popularity )

h2oTest <- as.h2o(testnnh2o)

#modélisation
pm.h2o <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,standardize = FALSE,activation="Maxout",hidden=c(2),seed=100,nfolds=5)

#prédiction
pred.pm.h20 <- h2o.predict(pm.h2o,newdata=h2oTest)

print(head(pred.pm.h20))
print(tail(pred.pm.h20))

#évaluation
cm<-as.matrix(pred.pm.h20)[,"predict"]
head(cm)
cm<-as.factor(cm)
confusionMatrix(testnnh2o$popularity ,cm )
#Ou
h2o.confusionMatrix(pm.h2o,newdata=h2oTest)

#0,5035 --> quasi tout prédit en moderatly

####-----
pm2.h2o <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,standardize = FALSE,activation="Maxout",hidden=c(20),seed=100,nfolds = 5 )

pred2.pm.h20 <- h2o.predict(pm2.h2o,newdata=h2oTest)
cm2<-as.matrix(pred2.pm.h20)[,"predict"]
cm2<-as.factor(cm2)
confusionMatrix(testnnh2o$popularity ,cm2 )

cmh2o<-h2o.confusionMatrix(pm2.h2o,newdata=h2oTest)
cmh2o

#0,5053 --> quasi tout prédit en moderatly

###------

#Random Hyper-Parameter Search

hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=list(2,3,4,5,6,7,8,9,10,15,20,25,30),
  input_dropout_ratio=c(0,0.05),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6)
)
hyper_params

## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
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
  nfolds=5
)                                
grid <- h2o.getGrid("dl_grid_random",sort_by="logloss",decreasing=FALSE)
grid

grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
best_model

pred3.pm.h20 <- h2o.predict(best_model,newdata=h2oTest)
cm3<-as.matrix(pred3.pm.h20)[,"predict"]
cm3<-as.factor(cm3)
confusionMatrix(testnnh2o$popularity ,cm3 )
#0.5131


####----

#Hyper-parameter Tuning with Grid Search

hyper_params <- list(
  hidden=list(2,3,4,5,6,7,8,9,10,15,20,25,30),
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6)
)
hyper_params
grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid", 
  training_frame=h2oTrain,  
  y="popularity",
  epochs=10,
  stopping_metric="misclassification",
  stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7, 
  l1=1e-5,
  l2=1e-5,
  activation=c("Rectifier"),
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params=hyper_params
)
grid

grid <- h2o.getGrid("dl_grid",sort_by="err",decreasing=FALSE)
grid

## To see what other "sort_by" criteria are allowed
#grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)

## Sort by logloss
h2o.getGrid("dl_grid",sort_by="logloss",decreasing=FALSE)

## Find the best model and its full set of parameters
grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]])
best_model

print(best_model@allparameters)
print(h2o.performance(best_model, valid=T))
print(h2o.logloss(best_model, valid=T))


pred4.pm.h20 <- h2o.predict(best_model,newdata=h2oTest)
cm4<-as.matrix(pred4.pm.h20)[,"predict"]
cm4<-as.factor(cm3)
confusionMatrix(testnnh2o$popularity ,cm4 )


####----

pm2c.h2o <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,standardize = FALSE,activation="Maxout",hidden=c(30,10,3),seed=100,nfolds = 5 )

pred2c.pm.h20 <- h2o.predict(pm2c.h2o,newdata=h2oTest)
cm2c<-as.matrix(pred2c.pm.h20)[,"predict"]
cm2c<-as.factor(cm2c)
confusionMatrix(testnnh2o$popularity ,cm2c )

#Maxout
#2à,10 : 0,2588
#20,5 : 0,2959
#20,3  : 0,483
#10,3 : 0,3412
#25, 3 : 0,3352
#15,3: 0,3194
#20,10,3 : 0,3534
#30,20,3 : 0,498
#40,20,3 : 0,4911
#35,20,3 : 0,2823
#30,10,3 : 0,5048
#30,15,3 : 0,2438
#3à,6,3 : 0,4984
#30,9,3 : 0,3449
#30,3 : 0,3397

#rectifier : 
##30,10,3 : 0,2434



#arrêt
h2o.shutdown()


#---------------NN Clément ----------------------------
# load library
install.packages("neuralnet")
library(neuralnet)

# fit neural network
nn=neuralnet(popularity~.,data=train[,50:59], hidden=2,linear.output = FALSE)

#Predict using Neural Network
predYnn = compute(nn, test[-59])

print(predYnn)

confusionMatrix(factor(predYnn),factor(y_test))
