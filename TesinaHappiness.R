rm(list=ls())

library(rstudioapi)
current_path<-getActiveDocumentContext()$path
setwd('C:/Users/galas/OneDrive - Università Politecnica delle Marche/Desktop/Dataset tesina MSBD')
print(getwd())
library(dplyr)
library(tidyverse)
library(GGally)
library(xlsx)
library(carData)
library(data.table)
library(ggplot2)
library(ggrepel)

# Studio e pulizia dataset

hac<-read.csv("HappinessAlcoholConsumption.csv")
View(hac)
hac$Hemisphere <- str_replace(hac$Hemisphere, "noth", "north")


hac_st<-scale(hac[,-c(1,2,3,6)])# hac scalato

#pie plot
tab<-hac$Region%>%table()
percentages<-tab%>%prop.table()%>%round(3)*100
percentages
txt<-paste0(names(tab), '\n', percentages,'%')
pie(tab, labels=txt)
#histogram
dev.new()
hac$Region%>%table()%>%barplot()
#abbelliamolo con i colori
bb<-hac$Region%>%table()%>%barplot(axisname=F, main="Species", ylab="frequency",
                                   col=c("pink","lightblue", "lightgreen"))
text(bb, tab/2, labels=txt, cex=1.06)


############PCA
rownames(hac)=hac[,1]

pc<-prcomp(hac[,-c(1,2,3,6)],scale=TRUE)

pc
plot(pc, type="l") # proprieta pc n.2
summary(pc)

biplot(pc)
abline(h=0)
abline(v=0)
pc$x #se utilizzo princomp pc$scores
pc$rotation #sono i miei autovettori o loadings

library(factoextra)
biplot(pc, col=c("grey", "orange"), cex=1, xlab='non sox',ylab='non soy')
abline(h=0)
abline(v=0)

fviz_pca_biplot(pc, repel = TRUE,
                col.var = "#FF5733", # Variables color
                col.ind = "696969"  # Individuals color
                
)



#scree plot per analizzare il peso delle componenti principali (Nord America)
fviz_eig(pc)

#rappresentazione di ogni valore(individuo) nel piano in base agli scores (Nord America)
disegno=data.frame(pc$x)
View(disegno)
p<-ggplot(disegno,aes(disegno[,1],disegno[,2]))+geom_label_repel(aes(label=row.names(hac)),size=2)
View(p)
p + ggtitle('Rappresentazione delle città sul piano in base agli scores') + xlab('qualità della vita') + ylab('tempo libero e cultura')







colSums(is.na(hac)) # non ci sono NA
library(data.table)
which(duplicated(hac))# NON ci sono duplicati



hac_2<-cbind(hac,pc$x[,1:2]) # aggiungo la prima e seconda cp a iris
View(hac_2)
cor(hac[,-c(1,2,3)],hac_2[,10:11])#vedo le correlazione tra pc e variabili mie non scalate

cor(hac_st,hac_2[,10:11])#vedo le correlazione tra pc e variabili mie non scalate





#correlations
cm1<-hac_st%>%as.matrix%>%cor()

library(ggcorrplot)
ggcorrplot(cm1)


##t=rho/sqrt(1-rho^2))*sqrt(n-2)

cor.test(hac$Beer_PerCapita, hac$HDI) # non fa



##########REGRESSIONE LINEARE
reglin_st=lm(HappinessScore~., data = data.frame(hac_st))
summary(reglin_st)
library(car)

vif(reglin_st)
plot(reglin_st)

# Crea un grafico a dispersione 3D

library(scatterplot3d)
library(plotly)

modello <- lm(HappinessScore ~ HDI + Beer_PerCapita, data = hac)

dev.new()
scatterplot3d(hac$HDI, hac$Beer_PerCapita, hac$HappinessScore,
              main = "Regressione lineare 3D", xlab = "HDI", ylab = "Beer_PerCapita", zlab = "HappinessScore",
              type = "p", color = "red")
plane3d(modello, draw_polygon = TRUE, polygon_args = list(col = "gray85"))

# Aggiungere una legenda
legend("bottomleft", legend = c("Punti dati", "Piano di regressione"), 
       col = c("red", "gray85"), pch = 16)

# Aggiungere linee di griglia
scatterplot3d(hac$HDI, hac$Beer_PerCapita, hac$HappinessScore,
              main = "Regressione lineare 3D", xlab = "HDI", ylab = "Beer_PerCapita", zlab = "HappinessScore",
              type = "p", color = "red", grid = TRUE)

# Aggiungere un titolo informativo
title(main = "Relazione tra HDI, Beer_PerCapita e HappinessScore")

# Cambiare il colore dei punti dati
scatterplot3d(hac$HDI, hac$Beer_PerCapita, hac$HappinessScore,
              main = "Regressione lineare 3D", xlab = "HDI", ylab = "Beer_PerCapita", zlab = "HappinessScore",
              type = "p", color = "blue", grid = TRUE)

library(plot3D)
library(rgl)
par(mar = c(2.1, 2.1, 4.1, 0.1))
planes3d(
  z = HappinessScore,
  x = HDI$seq,
  y = Beer_PerCapita$seq,
  zlab = "Predicted Vote Share",
  xlab = "Economic Growth",
  ylab = "Approval Rating",
  zlim = c(35, 75),
  xlim = growth_axis$range,
  ylim = approval_axis$range,
  cis = TRUE,
  xnlines = growth_axis$nlines,
  ynlines = approval_axis$nlines,
  main = "Incumbent Vote Shares, Economic \n Growth, and Approval Ratings",
  theta = -45,
  phi = 9,
  heatmap = joint_frequency
)
plot3D::points3D(
  z = hac$HappinessScore,
  x = hac$HDI,
  y = hac$Beer_PerCapita,
  add = TRUE,
  col = adjustcolor("black", alpha.f = .3),
  pch = 19
)
plot3D::text3D(
  z = hac$HappinessScore + 2.5,
  x = hac$HDI,
  y = hac$Beer_PerCapita,
  labels = hac$incumbent,
  add = TRUE,
  cex = 0.6
)


# Test di normalità dei residui
shapiro.test(resid(reglin_st))

# Test di omoschedasticità
library(car)
ncvTest(reglin_st)



#Analisi descrittiva con classifiche top e flop per ogni variabile

#HAPPINESS SCORE
hac_top25HS <- hac_2 %>% 
  arrange(desc(HappinessScore)) %>% 
  slice(1:25)

ggplot(hac_top25HS, aes(x=reorder(Country, HappinessScore), y=HappinessScore, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 25 Paesi per HappinessScore") +
  xlab("Country") +
  ylab("HappinessScore") +
  coord_flip() + 
  theme(legend.position = "none")

hac_flop25HS <- hac_2 %>% 
  arrange(desc(HappinessScore)) %>% 
  slice(97:122)

ggplot(hac_flop25HS, aes(x=reorder(Country, -HappinessScore), y=HappinessScore, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Flop 25 Paesi per HappinessScore") +
  xlab("Country") +
  ylab("HappinessScore") +
  coord_flip() + 
  theme(legend.position = "none")



#HDI
hac_top25HDI <- hac_2 %>% 
  arrange(desc(HDI)) %>% 
  slice(1:25)


ggplot(hac_top25HDI, aes(x=reorder(Country, HDI), y=HDI, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 25 Paesi per HDI") +
  xlab("Country") +
  ylab("HDI") +
  coord_flip() + 
  theme(legend.position = "none")

hac_flop25HDI <- hac_2 %>% 
  arrange(desc(HDI)) %>% 
  slice(97:122)

ggplot(hac_flop25HDI, aes(x=reorder(Country, -HDI), y=HDI, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Flop 25 Paesi per HDI") +
  xlab("Country") +
  ylab("HDI") +
  coord_flip() + 
  theme(legend.position = "none")


#GDP_PER_CAPITA
hac_top25GDP <- hac_2 %>% 
  arrange(desc(GDP_PerCapita)) %>% 
  slice(1:25)


ggplot(hac_top25GDP, aes(x=reorder(Country, GDP_PerCapita), y=GDP_PerCapita, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 25 Paesi per GDP_Per_Capita") +
  xlab("Country") +
  ylab("GDP_Per_Capita") +
  coord_flip() + 
  theme(legend.position = "none")

hac_flop25GDP <- hac_2 %>% 
  arrange(desc(GDP_PerCapita)) %>% 
  slice(97:122)

ggplot(hac_flop25GDP, aes(x=reorder(Country, -GDP_PerCapita), y=GDP_PerCapita, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Flop 25 Paesi per GDP") +
  xlab("Country") +
  ylab("GDP_Per_Capita") +
  coord_flip() + 
  theme(legend.position = "none")

#BEER_PER_CAPITA
hac_top25BEER <- hac_2 %>% 
  arrange(desc(Beer_PerCapita)) %>% 
  slice(1:25)


ggplot(hac_top25BEER, aes(x=reorder(Country, Beer_PerCapita), y=Beer_PerCapita, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 25 Paesi per Beer_Per_Capita") +
  xlab("Country") +
  ylab("Beer_Per_Capita") +
  coord_flip() + 
  theme(legend.position = "none")

hac_flop25BEER <- hac_2 %>% 
  arrange(desc(Beer_PerCapita)) %>% 
  slice(97:122)

ggplot(hac_flop25BEER, aes(x=reorder(Country, -Beer_PerCapita), y=Beer_PerCapita, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Flop 25 Paesi per BEER") +
  xlab("Country") +
  ylab("Beer_Per_Capita") +
  coord_flip() + 
  theme(legend.position = "none")

#WINE_PER_CAPITA
hac_top25WINE <- hac_2 %>% 
  arrange(desc(Wine_PerCapita)) %>% 
  slice(1:25)


ggplot(hac_top25WINE, aes(x=reorder(Country, Wine_PerCapita), y=Wine_PerCapita, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 25 Paesi per Wine_Per_Capita") +
  xlab("Country") +
  ylab("Wine_Per_Capita") +
  coord_flip() + 
  theme(legend.position = "none")

hac_flop25WINE <- hac_2 %>% 
  arrange(desc(Wine_PerCapita)) %>% 
  slice(97:122)

ggplot(hac_flop25WINE, aes(x=reorder(Country, -Wine_PerCapita), y=Wine_PerCapita, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Flop 25 Paesi per WINE") +
  xlab("Country") +
  ylab("Wine_Per_Capita") +
  coord_flip() + 
  theme(legend.position = "none")

#SPIRIT_PER_CAPITA
hac_top25SPIRIT <- hac_2 %>% 
  arrange(desc(Spirit_PerCapita)) %>% 
  slice(1:25)


ggplot(hac_top25SPIRIT, aes(x=reorder(Country, Spirit_PerCapita), y=Spirit_PerCapita, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 25 Paesi per Spirit_Per_Capita") +
  xlab("Country") +
  ylab("Spirit_Per_Capita") +
  coord_flip() + 
  theme(legend.position = "none")

hac_flop25SPIRIT <- hac_2 %>% 
  arrange(desc(Spirit_PerCapita)) %>% 
  slice(97:122)

ggplot(hac_flop25SPIRIT, aes(x=reorder(Country, -Spirit_PerCapita), y=Spirit_PerCapita, fill=Country)) + 
  geom_bar(stat = "identity") +
  ggtitle("Flop 25 Paesi per SPIRIT") +
  xlab("Country") +
  ylab("Spirit_Per_Capita") +
  coord_flip() + 
  theme(legend.position = "none")



## boxplot

boxplot(hac$HappinessScore)
boxplot(hac$HDI)
boxplot(hac$GDP_PerCapita)
boxplot(hac$Beer_PerCapita)
boxplot(hac$Spirit_PerCapita)
boxplot(hac$Wine_PerCapita)


# Impostazione della disposizione dei grafici
par(mfrow=c(2,3))

# Creazione dei boxplot
boxplot(hac$HappinessScore, main="Happiness Score")
boxplot(hac$HDI, main="HDI")
boxplot(hac$GDP_PerCapita, main="GDP per capita")
boxplot(hac$Beer_PerCapita, main="Beer per capita")
boxplot(hac$Spirit_PerCapita, main="Spirit per capita")
boxplot(hac$Wine_PerCapita, main="Wine per capita")



#density plot
media_hdi <- mean(hac_2$HDI)

# Crea il grafico di densità con ggplot
ggplot(data = hac_2, aes(x = HDI)) +
  geom_density() +
  # Aggiungi la linea verticale per la media
  geom_vline(xintercept = media_hdi, color = "red")


# Calcola la media di pressure
media_pressure <- mean(storms$pressure)

# Crea il grafico di densità con ggplot
ggplot(data = hac_2, aes(x = HDI)) +
  geom_density(fill = 'cyan') +
  # Aggiungi la linea verticale per la media
  geom_vline(xintercept = media_hdi, color = "red")



df <- tibble(x_variable = rnorm(5000), y_variable = rnorm(5000))

ggplot(hac_2, aes(x = HDI, y = HappinessScore)) +
  stat_density2d(aes(fill = ..density..), contour = F, geom = 'tile')

ggplot(hac_2, aes(x = Beer_PerCapita, y = HappinessScore)) +
  stat_density2d(aes(fill = ..density..), contour = F, geom = 'tile')


#Rinomino i continenti per comodità

hac$Region <- str_replace(hac$Region, "Australia and New Zealand", "ASIA")
hac$Region <- str_replace(hac$Region, "Eastern Asia", "ASIA")
hac$Region <- str_replace(hac$Region, "Southeastern Asia", "ASIA")

hac$Region <- str_replace(hac$Region, "Middle East and Northern Africa", "AFRICA")
hac$Region <- str_replace(hac$Region, "Sub-Saharan Africa", "AFRICA")

hac$Region <- str_replace(hac$Region, "Central and Eastern Europe", "EUROPA")
hac$Region <- str_replace(hac$Region, "Western Europe", "EUROPA")

hac$Region <- str_replace(hac$Region, "North America", "AMERICA")
hac$Region <- str_replace(hac$Region, "Latin America and Caribbean", "AMERICA")

#######CLUSTER ANALYSIS 
hac2 = hac[,-c(1,2,3)]

.rowNamesDF(hac, make.names=TRUE)<-as.vector(hac$Country)
.rowNamesDF(hac2, make.names=TRUE)<-as.vector(hac$Country)
hac_st<-scale(hac2)

dev.new()
boxplot(hac_st, xlab ="variabili", ylab="quantità")
boxplot(hac2, xlab ="variabili", ylab="quantità")

##DISTANZE
d1<-dist(hac2, method='euclidean')
summary(d1)
d<-dist(hac_st, method="euclidean")##meglio scalato
summary(d)

np=dim(hac_st)[1]
np

dev.new()
res=NbClust(hac_st, distance = "euclidean", min.nc=2, max.nc=11, method = "ward.D2", index = "all")
View(res)
View(res$Best.partition)

#DISTANZE CON L'ITALIA
sort(as.matrix(d)['Italy',])

dev.new()
dendro_ave = hclust(d,method='ward.D2')
plot(dendro_ave, main="Dendrogramma con 4 partizioni",labels=FALSE, xaxt="n", yaxt="n")
v=seq(from=1,to=(np-2), by=1)
taglio <-cutree(dendro_ave,k=4)
table(taglio)##numero individui in ogni gruppo
axis(1,v, labels=as.vector(hac$Country)[dendro_ave$order[v]],las=2,cex=0.07)
#abline(h=9, lty='dashed', col='blue')
rect.hclust(dendro_ave, k=4, taglio)#facciamo i quadrati dove tagliamo i grafici

##grafico con 3 partizioni
dev.new()
dendro_ave1 = hclust(d,method='ward.D2')
plot(dendro_ave1, main="Dendrogramma con 3 partizioni",labels=FALSE, xaxt="n", yaxt="n")
v=seq(from=1,to=(np-2), by=1)
taglio1 <-cutree(dendro_ave,k=3)
table(taglio1)##numero individui in ogni gruppo
axis(1,v, labels=as.vector(hac$Country)[dendro_ave1$order[v]],las=2,cex=0.07)
abline(h=15, lty='dashed', col='red')
rect.hclust(dendro_ave1, k=3, taglio1)#facciamo i quadrati dove tagliamo i grafici


dev.new()
plot(1:(np-1), dendro_ave$height, type ='b', xlab = 'Iterazione', ylab = 'distanza euclidea', main = 'happiness')#grafico delle distanze euclidee per tagliare meglio il grafico 
abline(h=9, lty='dashed', col='red')
abline(h=13, lty='dashed', col='blue')

g4<-cutree(dendro_ave, k = 4)# con k setti il numero di gruppi
g3<-cutree(dendro_ave, k = 3)# con k setti il numero di gruppi
g4
g3

centri4<-by(hac_st, g4, colMeans)##centroidi per ogni gruppo, la media deve essere fatta per gruppi. può essere fatta con il pipe operator. se analizzi bene ci sono i valori di deficit, debito, tasse, spesa ecc. puoi fare una analisi abbastanza approfondita su come sono strutturati i paesi all'interno dei gruppi
centri3<-by(hac_st, g3, colMeans)
centri3
centri4

#stampa i nomi dei gruppi
gruppi=rect.hclust(dendro_ave,k=4)
gruppi1=rect.hclust(dendro_ave1,k=3)


print(gruppi[[1]])
print(gruppi[[2]])
print(gruppi[[3]])
print(gruppi[[4]])

print(gruppi1[[1]])
print(gruppi1[[2]])
print(gruppi1[[3]])

summary(gruppi[[1]])
summary(gruppi[[2]])
summary(gruppi[[3]])
summary(gruppi[[4]])

summary(gruppi1[[1]])
summary(gruppi1[[2]])
summary(gruppi1[[3]])


##K-MEANS
###Silhouette
dev.new()
fviz_nbclust(hac_st, kmeans, method = "silhouette") +  labs(subtitle = "Elbow method")+ geom_vline(xintercept = 2, linetype = 2)#per k-means

# valuta la misura di silhouette al variare del numero di gruppi
for(k in 2:3){
  dev.new()
  plot(silhouette(pam(hac_st, k=k)), main = paste("k = ",k), do.n.k=FALSE)
  mtext("PAM(hac_st) as in Kaufman & Rousseeuw, p.101",
        outer = TRUE, font = par("font.main"), cex = par("cex.main")); 
  Sys.sleep(2)
}

##3 gruppi; analizziamo la partizione
dev.new()
nrip=50
ng=3
km3=kmeans(hac_st, centers = ng, nstart=nrip)#centers è il numero dei gruppi
table(hac$Country, km3$cluster)
fviz_cluster(km3, hac_st)
##con 2 clusters(suggerito)
dev.new()
nrip=50
ng_opt=2
km2=kmeans(hac_st, centers = ng_opt, nstart=nrip)#centers è il numero dei gruppi
table(hac$Country, km2$cluster)
fviz_cluster(km2, hac_st)
###possiamo calcolare la CH per i tre gruppi prendendo il loop (ciclo for sopra) e sostituisci km a km3
##anche per l'R2

by(hac_st, km3$cluster, colMeans)#centroidi dei gruppi 
km3$size ##non ci sono elephant clusters perche hanno più o meno tutti la stessa dimensione
km3$withinss/km3$size#rapporto per vedere quanto si distanziano i vari gruppi tra di loro


by(hac_st, km2$cluster, colMeans)
km2$size 
km2$withinss/km2$size

#devianza within e between
print(km3$withinss)
print(km3$betweenss)

print(km2$withinss)
print(km2$betweenss)

#statistica descritiva dei gruppi per le variabili
aggregate(hac_st[,-1],by=list(km2$cluster),FUN=mean, na.rm=TRUE)
aggregate(hac_st[,-1],by=list(km3$cluster),FUN=mean, na.rm=TRUE)


############################### PCA

pc<-prcomp(hac2,scale=TRUE)

dev.new()
plot(pc$x[,1:3], pch=20, col=km3$cluster, xlab='CP1', ylab='CP2', main='Felicità')
text(pc$x[,1:3], labels=hac$Country, pos=1, cex=0.6)
abline(h=0, v=0, lty='dotted')
legend('bottomleft', pch=20, col=1:3, legend=c('G1', 'G2', 'G3'))
points(cp_centri3[,1:2], pch='*', cex=2, col=1:3)

dev.new()
plot(pc$x[,c(1,2)], pch=20, col=km2$cluster, xlab='CP1', ylab='CP2', main='Felicità')
text(pc$x[,c(1,2)], labels=hac$Country, pos=1, cex=0.6)
abline(h=0, v=0, lty='dotted')
legend('bottomleft', pch=20, col=1:3, legend=c('G1', 'G2'))
points(cp_centri3[,c(1,3)], pch='*', cex=2, col=1:3)


############PCA
#scree plot per analizzare il peso delle componenti principali (Nord America)
dev.new()
fviz_eig(pc)

dev.new()
biplot(pc, cex = 0.7)
abline(h=0)
abline(v=0)
pc$x #se utilizzo princomp pc$scores 
pc$rotation #sono i miei autovettori o loadings

library(factoextra)
dev.new()
fviz_pca_biplot(pc, repel = TRUE,
                col.var = "#FF5733", # Variables color
                col.ind = "696969"  # Individuals color
)

#rappresentazione di ogni valore(individuo) nel piano in base agli scores
dev.new()
disegno=data.frame(pc$x)
View(disegno)
p<-ggplot(disegno,aes(disegno[,1],disegno[,2]))+geom_label_repel(aes(label=row.names(hac)),size=2)
View(p)
p + ggtitle('Classificazione dei paesi') + xlab('PC1') + ylab('Felicità e sviluppo umano')

hac_2<-cbind(hac,pc$x[,1:2]) # aggiungo la prima e seconda cp a iris
View(hac_2)
cor(hac[,-c(1,2,3)],hac_2[,9:10])#vedo le correlazione tra pc e variabili mie non scalate



library(pROC)

#REGRESSIONE LOGISTICA
hac$Hemisphere<-ifelse(hac$Hemisphere=="north",1,0)
library(aod)
library(lmtest)



#training and testing
set.seed(123)

#utilizzo l'80% dei dati come training e il 20% come testing
sample <- sample(c(TRUE, FALSE), nrow(hac), replace=TRUE, prob=c(0.75,0.25))
train <- hac[sample, ]
test <- hac[!sample, ]  


library(pscl)
library(car)
library(caret)
library(randomForest)
#Logisti regression applied to train dataset
model_full<-glm(Hemisphere~.,data=train, family="binomial")
logit1<-glm(Hemisphere ~  Beer_PerCapita + Spirit_PerCapita +HDI +HappinessScore ,data=train, family="binomial")
lrtest(model_full,logit1) #Since this p-value is not less than .05, we will fail to reject the null hypothesis.We use the nested model

logit2<-glm(Hemisphere ~  Beer_PerCapita + Spirit_PerCapita +HDI  ,data=train, family="binomial")
lrtest(logit1,logit2)# we use the nested model again

logit3<-glm(Hemisphere ~   Spirit_PerCapita +HDI  ,data=train, family="binomial")
lrtest(logit2,logit3)# this  time we fail to reject the null hipothesys so we use the model  logit2

library(aod)
wald.test(Sigma = vcov(logit2), b = coef(logit2), Terms = 1:3)#Since this p-value is less than .05,
#we reject the null hypothesis of the Wald test.
#this means that this coefficients are significant

summary(logit2)
pR2(logit2)["McFadden"]
vif(logit2) 
varImp(logit2) 



new <- data.frame(Beer_PerCapita = 300, Spirit_PerCapita = 300 , HDI=600) #VALORI bassi per tutte le variabili
predict(logit2, new, type="response")#mi dice che probabilita ha new di far parte del l'emisfero nord

predicted<-predict(logit2,test,type="response")# calcola la probabilita diogni individuo del  dataset test , di far parte del emisfero nord, basandoci sul modello del train
predicted<-as.data.frame((predicted))

#Roc curve
par(pty="s")

roc(test$Hemisphere, predicted$`(predicted)`, plot=TRUE,legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage",col="blue", lwd=4, print.auc=TRUE)
#Random Forest
rf.model <- randomForest(factor(Hemisphere) ~ Beer_PerCapita + Spirit_PerCapita +HDI,data=test)
plot.roc(test$Hemisphere, rf.model$votes[,1], percent=TRUE, col="darkgreen", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("blue", "darkgreen"), lwd=4)
par(pty="s")
#Finding optimal threshold using roc curve
roc.info <- roc(test$Hemisphere, predicted$`(predicted)`, legacy.axes=TRUE)
str(roc.info)


roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)


head(roc.df)
tail(roc.df)

roc.df[roc.df$tpp > 65 & roc.df$tpp < 76,]

#Settiamo il threshold e valutiamo la nostra matrice di confusione
predicted_b<-ifelse(predicted> 0.8638175,1,0) #Setto il cutoff
predicted_b<-as.data.frame((predicted_b))
colnames(predicted_b)[1] ="predicted_b_col"


bind<-cbind(test$Hemisphere,predicted_b)

#Confusion Matrix
test$Hemisphere<-as.factor(test$Hemisphere)
str(test$Hemisphere)
predicted_b$predicted_b_col<- as.factor(predicted_b$predicted_b_col)
str(predicted_b$predicted_b_col)



cf<-confusionMatrix( test$Hemisphere,predicted_b$predicted_b_col)
cf
#calculate precision
precision(test$Hemisphere, predicted_b$predicted_b_col)

#calculate sensitivity
sensitivity(test$Hemisphere, predicted_b$predicted_b_col)
#calculate specificity
specificity(test$Hemisphere, predicted_b$predicted_b_col)

dev.new()
fourfoldplot(as.table(cf), color = c("magenta", "green"), main = "Confusion Matrix")


#RANDOM FOREST

set.seed(42)

## NOTE: For most machine learning methods, you need to divide the data
## manually into a "training" set and a "test" set. This allows you to train 
## the method using the training data, and then test it on data it was not
## originally trained on. 
##
## In contrast, Random Forests split the data into "training" and "test" sets 
## for you. This is because Random Forests use bootstrapped
## data, and thus, not every sample is used to build every tree. The 
## "training" dataset is the bootstrapped data and the "test" dataset is
## the remaining samples. The remaining samples are called
## the "Out-Of-Bag" (OOB) data.

##data.imputed <- rfImpute(Hemisphere ~ ., data = hac, iter=6)
hac$Hemisphere <- as.character(hac$Hemisphere)
hac$Hemisphere <- as.factor(hac$Hemisphere)
model <- randomForest(Hemisphere ~ ., data=hac, proximity=TRUE)
model

# valutiamo gli errori commessi utilizzando un RF con 500 alberi
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "north", "south"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"north"], 
          model$err.rate[,"south"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))



#vediamo con 1000 alberi
model <- randomForest(Hemisphere ~ ., data=hac,ntree=1000, proximity=TRUE)

model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "north", "south"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"north"], 
          model$err.rate[,"south"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

tail(model$err.rate)
#vediamo con 1500
model <- randomForest(Hemisphere ~ ., data=hac,ntree=1500, proximity=TRUE)

model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "north", "south"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"north"], 
          model$err.rate[,"south"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

tail(model$err.rate)

dev.new()
varImpPlot(model)
## Blue line = The error rate specifically for calling "south" countries that
## are OOB.
##
## Green line = The overall OOB error rate.
##
## Red line = The error rate specifically for calling "north" countries 
## that are OOB.

#valutiamo il numero di variabili che vanno(mtry) prese all'inizio 
## If we want to compare this random forest to others with different values for
## mtry (to control how many variables are considered at each step)...
# oob.values <- vector(length=4)
# for(i in 1:4) {
#   temp.model <- randomForest(Hemisphere ~ ., data=hac, mtry=i, ntree=1500)
#   oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
# }
# 
# oob.values
# ## find the minimum error
# min(oob.values)
# ## find the optimal value for mtry...
# which(oob.values == min(oob.values))
# ## create a model for proximities using the best value for mtry
# #model <- randomForest(Hemisphere ~ ., 
#                       data=hac,
#                       ntree=1500, 
#                       proximity=TRUE, 
#                       mtry=2)
# 
# 
# model
#PCoa o MDS
distance.matrix <- as.dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=hac$Hemisphere)
row.names<-hac0$Country

ggplot(data=mds.data, aes(x=X, y=Y, label=hac0$Country)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")



# gli assi x e y rappresentano la percentuale della varianza della distanza
# da provare con dataset scalato e con happiness score che è molto correlato


#roc della random forest

par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:

roc(hac$Hemisphere,model$votes[,1] , plot=TRUE,legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage",col="brown", lwd=4, print.auc=TRUE)
