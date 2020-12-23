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


data$weekday_is_monday <- factor(data$weekday_is_monday) 
data$weekday_is_wednesday <- factor(data$weekday_is_wednesday) 
data$weekday_is_thursday <- factor(data$weekday_is_thursday) 
data$weekday_is_friday <- factor(data$weekday_is_friday) 
data$weekday_is_tuesday <- factor(data$weekday_is_tuesday) 
data$weekday_is_saturday <- factor(data$weekday_is_saturday) 
data$weekday_is_sunday <- factor(data$weekday_is_sunday) 

data$data_channel_is_lifestyle <- factor(data$data_channel_is_lifestyle) 
data$data_channel_is_entertainment <- factor(data$data_channel_is_entertainment) 
data$data_channel_is_bus <- factor(data$data_channel_is_bus) 
data$data_channel_is_socmed <- factor(data$data_channel_is_socmed) 
data$data_channel_is_tech <- factor(data$data_channel_is_tech) 
data$data_channel_is_world <- factor(data$data_channel_is_world)


#garder que les données quanti
data_qt <- data_inf_30k[,-c(18,20,24,37:42,60:63)]

#data avec selection de features 
data_qt2 <- data_inf_30k[,c(12:16,18:20,25:26,31:33,35:36,38,43,47,59)]

# 75% of the sample size
smp_size <- floor(0.75 * nrow(data_inf_30k))

# separation train/test avec une seed
set.seed(123)
train_ind <- sample(seq_len(nrow(data_inf_30k)), size = smp_size)

train <- data_qt[train_ind, ]

test <- data_qt[-train_ind, ]



y_train <- as.matrix(train[,50])
y_test <- as.matrix(test[,50])
x_train <- as.matrix(train[,-50])
x_test <- as.matrix(test[,-50])
#train<- as.data.frame(scale(train,center=T))
test[50] <- scale(test[50],center=T)
train[50]<- scale(train[50],center=T)
fit_full <- lm(shares ~ ., data = train)
summary(fit_full)
select_fit<-step(fit_full, k=log(nrow(train)), direction="backward")

summary(select_fit)
pred<- predict(select_fit,as.data.frame(test[,-50]))

#c'est nul mdr
plot(y_test)
points(pred,col = "red", pch=16)

#1-((1836+1566)/9803)

data.frame(
  RMSE = RMSE(pred, y_test),
  Rsquare = R2(pred, y_test)
)

sqrt(mean((test$shares - pred)^2))
summary(test$shares)

a <- ggplot(train, aes(x = shares))

g <- a + geom_histogram(bins = 5, color = "black", fill = "gray") + geom_vline(aes(xintercept = median(shares)), linetype = "dashed", size = 0.6)

ggplotly(g)
