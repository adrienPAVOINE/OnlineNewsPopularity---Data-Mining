
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

