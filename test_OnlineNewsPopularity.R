#packages
library(corrplot)


#import de données
data <-read.csv("C:/GITHUB/Data-Mining-Project/OnlineNewsPopularity/OnlineNewsPopularity.csv",sep=",",dec=".")

summary(data)

#statistiques sur les token

#mauvaise idée car si les graphs sont plus lisibles on perd l'info sur le nb de partage et de mots
#data_cr <- as.data.frame(scale(data[,3:61],center=T,scale=T))

#répartition normale, le nombre optimal de mots semble centré autour de 10-11
plot(x=data$n_tokens_title,y=data$shares)

#il semblerait que les articles courts soient plus partagés
plot(x=data$n_tokens_content,y=data$shares)
