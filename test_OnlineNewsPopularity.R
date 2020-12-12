#packages
library(corrplot)
library(ggplot2)
library(plotly)
#import de donn�es
data_all <-read.csv("C:/GITHUB/Data-Mining-Project/OnlineNewsPopularity/OnlineNewsPopularity.csv",sep=",",dec=".")


###########################
#traitements de donn�es
###########################

#Suppression d'une ligne avec un ratio de mots unique � 700 + suppression url et timedelta
data <- data_all[-31038,-c(1:2)]
summary(data)


###########################
#1ere approche globale
###########################
#39644 articles dans la base

#en moyenne 3395 partages par article mais il existe des articles tr�s peu ou tr�s publi�s
summary(data$shares)


# avec un histogramme on se rend compte que 38 000 articles sont dans la premi�res classes
# donc on a � peine 2000 articles qui ont plus de 30 000 partages
# m�me en bougeant les classes on constate le m�me r�sultat

a <- ggplot(data, aes(x = shares))

g <- a + geom_histogram(bins = 30, color = "black", fill = "gray") + geom_vline(aes(xintercept = median(shares)), linetype = "dashed", size = 0.6)

ggplotly(g)

#On va concentrer l'analyse sur les articles de - de 30 000 partages pour regarder plus en d�tails la r�partition

data_inf_30k <- data[which(data$shares<30000), ]


# on obtient des r�sultats plus clair, presque la moiti� des articles ont entre 1000 et 2000 partages
a <- ggplot(data_inf_30k, aes(x = shares))

g <- a + geom_histogram(bins = 50, color = "black", fill = "gray") + geom_vline(aes(xintercept = median(shares)), linetype = "dashed", size = 0.6)

ggplotly(g)

#bon apr�s tout �a on pouvait le savoir gr�ce au summary (en regardant les quartils)

#a priori pas de grosses corr�lation entre une variable et le nombre de partages
d<-as.data.frame(cor(data))
print(d['shares'])


###########################
#statistiques sur les token
###########################

#mauvaise id�e car si les graphs sont plus lisibles on perd l'info sur le nb de partage et de mots
#data_cr <- as.data.frame(scale(data[,3:61],center=T,scale=T))

#r�partition normale, le nombre optimal de mots semble centr� autour de 10-11
plot(x=data$n_tokens_title,y=data$shares)

#il semblerait que les articles courts soient plus partag�s
plot(x=data$n_tokens_content,y=data$shares)

#pas vraiment d'informations � voir
plot(x=data$n_unique_tokens,y=data$shares)

#a priori pas de grosse corr�lation
corrplot(cor(data[,c(3:5,61)]))

