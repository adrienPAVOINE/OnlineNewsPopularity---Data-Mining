#packages
library(corrplot)
library(ggplot2)
library(plotly)
#import de données
data_all <-read.csv("C:/GITHUB/Data-Mining-Project/OnlineNewsPopularity/OnlineNewsPopularity.csv",sep=",",dec=".")


###########################
#traitements de données
###########################

#Suppression d'une ligne avec un ratio de mots unique à 700 + suppression url et timedelta
data <- data_all[-31038,-c(1:2)]
summary(data)


###########################
#1ere approche globale
###########################
#39644 articles dans la base

#en moyenne 3395 partages par article mais il existe des articles très peu ou très publiés
summary(data$shares)


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

g <- a + geom_histogram(bins = 50, color = "black", fill = "gray") + geom_vline(aes(xintercept = median(shares)), linetype = "dashed", size = 0.6)

ggplotly(g)

#bon après tout ça on pouvait le savoir grâce au summary (en regardant les quartils)

#a priori pas de grosses corrélation entre une variable et le nombre de partages
d<-as.data.frame(cor(data))
print(d['shares'])


###########################
#statistiques sur les token
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
corrplot(cor(data[,c(3:5,61)]))


