
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
#data_all <-read.csv("C:/GITHUB/Data-Mining-Project/OnlineNewsPopularity/OnlineNewsPopularity.csv",sep=",",dec=".")
data_all <-read.csv("/Volumes/KINGSTON/M2/JAP/Projet_JAP/git/Data-Mining-Project/OnlineNewsPopularity.csv",sep=",",dec=".")


###########################
#traitements de données   ####################################################################
###########################

# checking for NaN
sum(is.na(data))

#Suppression d'une ligne avec un ratio de mots unique à 700 + suppression url et timedelta
# The following variables can be omitted:
#   
# url: URL of the article (non-predictive)
# timedelta: Days between the article publication and the dataset acquisition (non-predictive)
# five LDA variables
# is_weekend, since it seems to be duplicating days of week
# kw_min_min, kw_avg_min, kw_min_avg have a number of negative values

data <- data_all[-31038,-c(1:2,20,22,26,39:44)]

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

correlation<- as.data.frame(cor(data[,-c(51:54)]))

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
corrplot(cor(data[,c(1:3,50)]))


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
#Préparation des echantillons          ####################################################################
#########################################



#garder que les données quanti
data_qt <- data[,-c(50,52:54)]

#data avec selection de features 
fit_full <- lm(shares ~ ., data = data[,-c(51:54)])
select_fit<-step(fit_full, k=log(nrow(train)), direction="both")
data_qt2 <- data[,c(labels(select_fit$coefficients)[-1],"popularity")]

# 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data_qt[train_ind, ]

test <- data_qt[-train_ind, ]

y_train <- as.matrix(train[,50])
y_test <- as.matrix(test[,50])
x_train <- as.matrix(as.matrix(scale(train[,-50],center = T)))
x_test <- as.matrix(as.matrix(scale(test[,-50],center = T)))

#########################################
#Modèle de classification pénalisé      ####################################################################
#########################################


# 
# # modèle LASSO
# 
# reg <- glmnet(x_train,y_train,family="multinomial",alpha=1,standardize=FALSE,type.multinomial = "grouped",lambda=0)
# 
# # Display regression coefficients
# print(coef(reg))
# 
# # prediction sur l'échantillon 
# 
# pred<- predict(reg,x_test,type="class")
# 
# 
# #resultats
# confusionMatrix(factor(pred),factor(y_test))
# 
# # modèle RIDGE
# 
# reg <- glmnet(x_train,y_train,family="multinomial",alpha=0,standardize=FALSE,type.multinomial = "grouped",lambda=0)
# 
# # Display regression coefficients
# print(coef(reg))
# 
# # prediction sur l'échantillon 
# 
# pred<- predict(reg,x_test,type="class")
# 
# 
# #resultats
# confusionMatrix(factor(pred),factor(y_test))


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
confusionMatrix(factor(pred),factor(y_test))

confusionMatrix(factor(pred2),factor(y_test))

#0.5167 contre 0.5127

#on va essayer avec la selection de variables

train <- data_qt2[train_ind, ]

test <- data_qt2[-train_ind, ]

y_train <- as.matrix(train[,11])
y_test <- as.matrix(test[,11])
x_train <- as.matrix(as.matrix(scale(train[,-11],center = T)))
x_test <- as.matrix(as.matrix(scale(test[,-11],center = T)))


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

pred3<- predict(reg,x_test,type="class")
pred4 <-predict(reg2,x_test,type="class")

#resultats
confusionMatrix(factor(pred),factor(y_test))

confusionMatrix(factor(pred2),factor(y_test))
#0.5125 contre 0.5101

timing <- function(expr) system.time(expr)[3]
accuracy <- function(ConfMat){return(ConfMat$overall[1])}


#test de fonction de comparaison

res_all <- replicate(200, simplify = FALSE, {

  t_all <- timing(mod_all <- glmnet(X[ind.train, ], y[ind.train]))
  
  preds_all <- predict(mod_all, X[ind.test, ])
  rmse_all <- apply(preds_all, 2, RMSE, y[ind.test])
  
  t_cv <- timing(mod_cv <- cv.glmnet(X[ind.train, ], y[ind.train]))
  preds_1se <- predict(mod_cv, X[ind.test, ], s = "lambda.1se")
  rmse_1se <- RMSE(preds_1se, y[ind.test])
  preds_min <- predict(mod_cv, X[ind.test, ], s = "lambda.min")
  rmse_min <- RMSE(preds_min, y[ind.test])
  
  library(bigstatsr)
  t_CMSA <- timing({
    X2 <- as_FBM(X)
    mod_CMSA <- big_spLinReg(X2, y[ind.train], ind.train)
  })
  preds_CMSA <- predict(mod_CMSA, X2, ind.test)
  rmse_CMSA <- RMSE(preds_CMSA, y[ind.test])
  
  library(glmnetUtils)
  ALPHA <- c(1, 0.5, 0.1)
  t_cva <- timing(mod_cva <- cva.glmnet(X[ind.train, ], y[ind.train], alpha = ALPHA))
  alpha <- ALPHA[which.min(sapply(mod_cva$modlist, function(mod) min(mod$cvm)))]
  rmse_cva <- RMSE(predict(mod_cva, X[ind.test, ], alpha = alpha), y[ind.test])
  
  t_CMSA2 <- timing({
    X2 <- as_FBM(X)
    mod_CMSA2 <- big_spLinReg(X2, y[ind.train], ind.train, alphas = ALPHA)
  })
  preds_CMSA2 <- predict(mod_CMSA2, X2, ind.test)
  rmse_CMSA2 <- RMSE(preds_CMSA2, y[ind.test])
  
  tibble::tribble(
    ~method,        ~timing,  ~rmse,
    "glmnet_best",  t_all,    min(rmse_all),
    "glmnet_min",   t_cv,     rmse_1se,
    "glmnet_1se",   t_cv,     rmse_min,
    "CMSA",         t_CMSA,   rmse_CMSA,
    "glmnet_cva",   t_cva,    rmse_cva,
    "CMSA2",        t_CMSA2,  rmse_CMSA2
  )
  
})

res <- do.call(rbind, res_all)
res$run_number <- rep(seq_along(res_all), each = 6)



#########################################
#Support Vector Machine                 ####################################################################
#########################################

#Install Package
install.packages("e1071")

#Load Library
library(e1071)

#test d'un SVM basique

#centrage reduction des variables explicatives
#train[-59] <- scale(train[-59],center=T)
#train[,59] <- as.factor(train[,59])
#test[-59] <- scale(test[-59],center=T)
#test[,59] <- as.factor(test[,59])

#avant de lancer le svm il faut absolument réduire le nombre de features sinon le temps d'exécution va etre abominable
#temps avec selection de features 2-5 minutes
#temps modèle complet 15-20 minutes


#Regression with SVM
#modelsvm = svm(popularity~.,train[,c(12:16,18:20,25:26,31:33,35:36,38,43,47,59)],kernel="radial",type="C-classification")

#modelsvm = svm(popularity~.,train,kernel="radial",type="C-classification")

#Predict using SVM regression
#predYsvm = predict(modelsvm, test[-59])

#resultats
#confusionMatrix(factor(predYsvm),factor(y_test)) 




#oc_svm_pred_test <- predict(modelsvm,test[-59],decision.values = TRUE)

#Réalisation d'un SVM avec 

newdata <- data_qt[,c(12:16,18:20,25:26,31:33,35:36,38,43,47,59)]
# 75% of the sample size
smp_size <- floor(0.75 * nrow(newdata))
# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(newdata)), size = smp_size)
ntrain <- newdata[train_ind, ]

ntest <- newdata[-train_ind, ]

#centrage reduction des variables explicatives
ntrain[-19] <- scale(ntrain[-19],center=T)
ntrain[,19] <- as.factor(ntrain[,19])
ntest[-19] <- scale(ntest[-19],center=T)
ntest[,19] <- as.factor(ntest[,19])
svmfit =svm(popularity~., data=ntrain, kernel ="radial", type="C-classification",gamma =0.1,cost =10)
#réalisation de la prédiction
pred = predict(svmfit, ntest[-19])
table(pred, ntest[,19])
#on constate que presque tout est prédit populaire mais un peu mieux que précedant
#resultats
confusionMatrix(factor(pred),factor(ntest[,19])) 
#on obtient un taux d'accuracy de 0.5113

######tunage des paramètres#####
#tunage de gamma et cost####
#trop long à executer 
svm_tune <- tune(svm, train.x=ntrain[-19], train.y=ntrain[,19], kernel="radial", ranges=list(cost=10^(-2:2), gamma=2^(-2:2)))


####Réduction avec une ACP########

#On essaye de réduire les dimensions pour ne retenir que 2 axes 
library("FactoMineR")
library("factoextra")

res.pca <- PCA(newdata[-19], scale.unit = TRUE, ncp = 2, graph = TRUE)
ind <- get_pca_ind(res.pca)
daf <- data.frame(cbind(ind$coord, newdata$popularity))
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
#on réalise le modele sur ces doonnées
svmfit =svm(V3~., data=ftrain, kernel ="radial", type="C-classification",gamma =0.1,cost =10)
#avec 2 dimensions, on peut afficher un graphique 
plot(svmfit, ftrain, Dim.1 ~ Dim.2,
     slice=list(Dim.1=3, Dim.2=4))
#réalisation de la prédiction
pred = predict(svmfit, ftest[-3])
table(pred, ftest[,3])
#on constate que presque tout est prédit populaire
#resultats
confusionMatrix(factor(pred),factor(ftest[,3])) 
#on obtient un taux d'accuracy de 0.5062


#Réalise le même test avec toutes les données quanti et en gardant 5 axes
res.pca <- PCA(data_qt[-59], scale.unit = TRUE, ncp = 5, graph = TRUE)
ind <- get_pca_ind(res.pca)
daf <- data.frame(cbind(ind$coord, newdata$popularity))
daf[,1] <- round(as.double(daf[,1]),3)
daf[,2] <- round(as.double(daf[,2]),3)
daf[,3] <- round(as.double(daf[,3]),3)
daf[,4] <- round(as.double(daf[,4]),3)
daf[,5] <- round(as.double(daf[,5]),3)
plot(daf$Dim.1,daf$Dim.2)
# 75% of the sample size
smp_size <- floor(0.75 * nrow(daf))
# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(daf)), size = smp_size)
ftrain <- daf[train_ind, ]
ftest <- daf[-train_ind, ]

ftrain[,6] <- as.factor(ftrain[,6])
ftest[,6] <- as.factor(ftest[,6])
#on réalise le modele sur ces doonnées
svmfit =svm(V6~., data=ftrain, kernel ="radial", type="C-classification",gamma =0.1,cost =10)
#avec 2 dimensions, on peut afficher un graphique 
plot(svmfit, ftrain, Dim.1 ~ Dim.2,
     slice=list(Dim.1=3, Dim.2=4))
#réalisation de la prédiction
pred = predict(svmfit, ftest[-6])
table(pred, ftest[,6])
#on constate que presque tout est prédit populaire mais un peu mieux que précedant
#resultats
confusionMatrix(factor(pred),factor(ftest[,6])) 
#on obtient un taux d'accuracy de 0.51



###autres tests

obj <- tune(svm, V3~., data = ftrain,
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix"))


tuned_parameters <- tune.svm(popularity~., data = ntrain, gamma = 10^(-5:-1), cost = 10^(-3:1))
summary(tuned_parameters )

svmfit =svm(popularity~., data=ntrain, kernel ="radial", type="C-classification",gamma =0.5,cost =1)
summary(obj)
plot(obj)
#model = svm(X, Y, type = "C-classification", kernel = "polynomial")

pred = predict(svmfit, ftest[-3])

table(pred, ftest[,3])

#########################################
#        NEURAL NETWORK                 ####################################################################
#########################################

#--------------NN Antho-----------------------

library(UBL)

tk<-cbind(xsmote,ysmote)

balanced.data <- TomekClassif(ysmote~., tk, dist = "Euclidean", p = 2, Cl = c("Moderatly popular"), rem = "maj" )  #c("Very popular","Moderatly popular","Not very Popular")
bdata_1 <- balanced.data[[1]]
plot(bdata_1[,c(1,2)], col = ifelse(bdata_1$Species == "rare", "red","blue"), pch=16, cex
= 2 , xlim = c(4,8), ylim = c(2,4.2))


library(ROSE)
dunder<-cbind(xsmote,ysmote)
tmp_df = dunder[which(ysmote=="Moderatly popular"),]
tmp<-ovun.sample(ysmote ~ ., data = tmp_df, method = "under", p = 0.5, seed = 5)$data
balanced_sample<-rbind(balanced_sample, tmp)

library(smotefamily)

xsmote<-as.data.frame(x_train)
ysmote<-as.factor(y_train)
length(ysmote)

balanced.data <- SMOTE(X=xsmote, target = ysmote ,K =5, dup_size = 1 ) 
new_data = balanced.data$syn_data
bdata = balanced.data$data
table(ysmote)
table(bdata$class)

balanced.data2 <- SMOTE(X=bdata[,1:58], target = bdata[,59] ,K =5, dup_size = 1 ) 
new_data2 = balanced.data2$syn_data
bdata2 = balanced.data2$data
table(ysmote)
table(bdata$class)
table(bdata2$class)



h2o.init(nthreads = -1)

#transformer en un format reconnu par h2o
h2oTrain<-bdata2
h2oTrain$class <- as.factor(h2oTrain$class)
h2oTrain <- as.h2o(h2oTrain)

testnnh2o <- test
testnnh2o$popularity <- as.factor(testnnh2o$popularity )

h2oTest <- as.h2o(testnnh2o)

#modélisation
pm.h2o <- h2o.deeplearning(y="class",training_frame = h2oTrain,standardize = FALSE,activation="Maxout",hidden=c(30,10),seed=100,nfolds=5)

#prédiction
pred.pm.h20 <- h2o.predict(pm.h2o,newdata=h2oTest)

#évaluation
cm<-as.matrix(pred.pm.h20)[,"predict"]
head(cm)
cm<-as.factor(cm)
confusionMatrix(testnnh2o$popularity ,cm )
#Ou
h2o.confusionMatrix(pm.h2o,newdata=h2oTest)


## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_random",
  training_frame=h2oTrain,
  y="class",
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




#____________LIBRARY NNET______________________

library(nnet)
set.seed(100)

ciblennet <- as.factor(train[,'popularity'])

ps.nnet <- nnet(ciblennet ~ ., data = x_train, skip = F, size = 3,na.action = na.omit, maxit=300 )

#100,150,200 pas convergé
#300 ok 

#prediction
pred.ps.nnet <- predict(ps.nnet,newdata=test, type="class")

pred.ps.nnet<-as.factor(pred.ps.nnet)
confusionMatrix(as.factor(test$popularity) ,pred.ps.nnet )

table(test$popularity,pred.ps.nnet)

#Quasi tout prédit en not very 

ciblennet <- as.factor(train[,'popularity'])

ps.nnet3 <- nnet(ciblennet ~ ., data = x_train, skip = F, size =10,na.action = na.omit, maxit=300 )

#prediction
pred.ps.nnet3 <- predict(ps.nnet3,newdata=test, type="class")

pred.ps.nnet3<-as.factor(pred.ps.nnet3)
confusionMatrix(as.factor(test$popularity) ,pred.ps.nnet3)

#quasi tout en moderatly 


ciblennet2<-as.factor(bdata2[,59])
ps.nnet2 <- nnet(ciblennet2 ~ ., data = bdata2, skip = F, size = 3,na.action = na.omit, maxit=500 )

#prediction
pred.ps.nnet2 <- predict(ps.nnet2,newdata=test, type="class")

pred.ps.nnet2<-as.factor(pred.ps.nnet2)
confusionMatrix(as.factor(test$popularity) ,pred.ps.nnet2 )





#____________LIBRARY H2O______________________

library(h2o)

trainnnh2o <- train
trainnnh2o$popularity <- as.factor(trainnnh2o$popularity )

#initialisation - nthreads = -1, utiliser tous les c??urs disponibles
h2o.init(nthreads = -1)

#transformer en un format reconnu par h2o
h2oTrain <- as.h2o(trainnnh2o)

testnnh2o <- test
testnnh2o$popularity <- as.factor(testnnh2o$popularity )

h2oTest <- as.h2o(testnnh2o)

#modélisation
pm.h2o <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,activation="Maxout",hidden=c(2),seed=100,nfolds=5,
                           keep_cross_validation_predictions=T,keep_cross_validation_fold_assignment=T)

#prédiction
pred.pm.h20 <- h2o.predict(pm.h2o,newdata=h2oTest)

#print(head(pred.pm.h20))
#print(tail(pred.pm.h20))

#évaluation
cm<-as.matrix(pred.pm.h20)[,"predict"]
#head(cm)
cm<-as.factor(cm)
confusionMatrix(testnnh2o$popularity ,cm )
#Ou
h2o.confusionMatrix(pm.h2o,newdata=h2oTest)



####-----
pm2.h2o <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,activation="Maxout",hidden=c(20),seed=100,nfolds = 5,
                            keep_cross_validation_predictions=T,keep_cross_validation_fold_assignment=T)

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
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout")#,
 # hidden=list(2,3,4,5,6,7,8,9,10,15,20,25,30)#,
  #input_dropout_ratio=c(0,0.05),
  #l1=seq(0,1e-4,1e-6),
 # l2=seq(0,1e-4,1e-6)
)
#hyper_params

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
table(testnnh2o$popularity ,cm3 )
table(cm3)
table(testnnh2o$popularity)

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

pm2c.h2o <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,standardize = FALSE,activation="Maxout",hidden=c(30,10,3),seed=100,nfolds = 5,
                             keep_cross_validation_predictions=T,keep_cross_validation_fold_assignment=T)

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


varimp<-as.data.frame(h2o.varimp(pm2c.h2o))
varimp
varbest<-varimp$variable[varimp$relative_importance>0.7] #.7

trainnnh2o2 <- train[,c(varbest,'popularity')]
trainnnh2o2$popularity <- as.factor(trainnnh2o2$popularity )

#transformer en un format reconnu par h2o
h2oTrain2 <- as.h2o(trainnnh2o2)

testnnh2o2 <- test[,c(varbest,'popularity')]
testnnh2o2$popularity <- as.factor(testnnh2o2$popularity )

h2oTest2 <- as.h2o(testnnh2o2)

pm2c.h2o <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,standardize = FALSE,activation="Maxout",hidden=c(30,10,3),seed=100,nfolds = 5,
                             keep_cross_validation_predictions=T,keep_cross_validation_fold_assignment=T)

pred2c.pm.h20 <- h2o.predict(pm2c.h2o,newdata=h2oTest)
cm2c<-as.matrix(pred2c.pm.h20)[,"predict"]
cm2c<-as.factor(cm2c)
confusionMatrix(testnnh2o$popularity ,cm2c )


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


#########################################
#        RANDOM FOREST                  ####################################################################
#########################################

install.packages("randomForest")
library(randomForest)
data_qt <- data[,-c(50,52:54)]
data_qt$popularity<-factor(data_qt$popularity)
RandomForest_News <- randomForest(popularity~.,data=data_qt, ntree = 200, 
                                  mtry = 3, na.action = na.roughfix)

print(RandomForest_News)

x <- data_qt[,1:49]
y <- data_qt[,50]

# Random Search
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
set.seed(123)
mtry <- 10
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=mtry)
rf_random <- train(popularity~., data=data_qt, method="rf", metric=metric, trControl=control)
print(rf_random)
plot(rf_random) 