###########################
#TEST TRAITEMENTS DATA    ####################################################################
###########################

data$popularity[data$shares < 946.1] <- "Unpopular"
data$popularity[(data$shares > 946.1 & data$shares < 1400.1) ] <- "Moderatly popular"
data$popularity[(data$shares > 1400.1 & data$shares < 2800.1)] <- "Quite popular"
data$popularity[data$shares > 2800.1] <- "Very popular"

data$popularity[data$shares < 1400.1] <- "Low"

data$popularity[data$shares > 1400.1] <- "High"

#########################################
#   TEST SELECTION FEATURES             ####################################################################
#########################################
#rien de fou, on va faire une selection de variable

# selection de variables vraiment pas folle
mod <- lm(popularity~.,data=data_qt)
print(mod)
step(mod, data=data_qt,direction="backward")

# on va tenter une ACP pour voir les variables qui contribuent le plus

#mise en place de l'acp avec l'option pour centrer r?duire les donn?es
res.pca <- PCA(data_qt, scale.unit = TRUE, graph = T)
print(res.pca)
# graphique ?cras? car 2 individus contribuent tr?s fortement ? la construction de l'axe 2

eig.val <- get_eigenvalue(res.pca) 
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


var <- get_pca_var(res.pca)
var

#-> Graphique du Cos2 des variables sur le 1 premier axe : 

fviz_cos2(res.pca, choice = "var", axes=3)

var$coord
var$cos2
var$contrib


###########################
# TEST REGRESSION         ####################################################################
###########################

print(as.data.frame(pred)[1])


#c'est nul mdr
plot(y_test)
points(pred,col = "red", pch=16)

#1-((1836+1566)/9803)

data.frame(
  RMSE = RMSE(pred, y_test),
  Rsquare = R2(pred, y_test)
)

