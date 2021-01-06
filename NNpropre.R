#------------------RESEAU DE NEURONNES----------------------

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
h2o.init(nthreads = -1)

#Transformation format reconnu par h2o
h2oTrain <- as.h2o(trainnnh2o)

#Memes manipulations pour l'echantillon test 
testnnh2o <- test
testnnh2o$popularity <- as.factor(testnnh2o$popularity )

h2oTest <- as.h2o(testnnh2o)

#Modélisation : 1 cocuhe cachee avec 3 neuronnes
# - Validation croisee avec nfolds 
# - On garde les prédictions de la validation croisee : keep_cross_validation_predictions
# - Fonction d'activation similaire à sigmoid : Tanh
pm.h2o <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,activation="Tanh",hidden=c(3),nfolds=5,
                           keep_cross_validation_predictions=T)#,seed=100

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


#Accuracy entre [0,509 ; 0,5288]  --> plutôt moyen 
#[0,5121 ; 0,5318]  0,5219  
#Beaucoup de prédictions en "Moderatly popular" 


#---Recherche d'hyper parametres aléatoire 

#On veut tester plusieurs fonctions d'activation et faire varier le nombre de neuronnes de la couche cachée 
hyper_params <- list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
   hidden=list(2,3,4,5,6,7,8,9,10,15,20,25,30)
)

#https://docs.h2o.ai/h2o-tutorials/latest-stable/tutorials/deeplearning/index.html
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
# couche cacchee: 9 neuronnes et Tanh, sortie : Softmax

#Predictions avec le meilleur modele

pred_bestnn <- h2o.predict(best_nn,newdata=h2oTest)
pred_bestnn_mat<-as.matrix(pred_bestnn)[,"predict"]
pred_bestnn_mat<-as.factor(pred_bestnn_mat)
cm_bestnn<-confusionMatrix(pred_bestnn_mat,testnnh2o$popularity)
print(cm_bestnn)
#Differentes metriques 
print(cm_bestnn$byClass)

#Accuracy [0.5084, 0.5282]
#Amelioration par rapport au premier essai ? --> très peu de not very popular en very et inversement


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

nn_h2o_bis <- h2o.deeplearning(y="popularity",training_frame = h2oTrain,activation="Tanh",hidden=c(40,10),nfolds=5,
                           keep_cross_validation_predictions=T) #,seed=100

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
#  (0.5152, 0.5349) 0.5251    40,10
#  (0.5132, 0.5329) 0.5231    41,10
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



#arrêt de H2o
h2o.shutdown()
