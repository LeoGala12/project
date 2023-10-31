rm(list=ls())
################################

#1)PULIZIA DATASET

setwd('C:/Users/39370/Desktop/tesina1')

tesina<-read.csv("CarPrice_Assignment.csv",header=TRUE)
View(tesina)

#provo a cancellare righe

View(tesina)

tesina=data.frame(tesina[-c(4,5,11,14,32,33,38,43,45,52,54,55,56,57,58,60,80,81,82,83,85,92,93,99,111,112,113,114,116,117,119,128,133,134,136,139,140,149,150,153,157,159,160,163,164,165,170,176,177,180,189,195,196,197,199),])
View(tesina)

dim(tesina)

row.names(tesina)=seq(1,150)
View(tesina)
#seconda mandata
tesina=data.frame(tesina[-c(27,63,113,119,123,125),])
View(tesina)

dim(tesina)

row.names(tesina)=seq(1,144)
View(tesina)





#cancello colonne

df=data.frame(tesina[,-1:-9])
View(df)
df=data.frame(df[,-6:-7])
View(df)
df=data.frame(df[-7])

View(df)

hist(df$compressionratio)

############################################

#2) REGRESSIONE LINEARE


df_st<-scale(df,center = TRUE,scale = TRUE)
df_st2=data.frame(df_st)

View(df_st2)


hist(df_st2$price)



reglin=lm(price~., data = df_st2)
summary(reglin)

reglin2=lm(price~horsepower+carlength+compressionratio+enginesize, data = df_st2)
summary(reglin2)


library(car)
vif(reglin2)

mean(reglin2$residuals)

library(olsrr)
#qqplot
ols_plot_resid_qq(reglin2)

ols_test_normality(reglin2)

res <- resid(reglin2)

#Produce residual vs. fitted plot
plot(fitted(reglin2), res)
abline(0,0)

#grafico a campana dei residui
plot(density(res))

#grafico dipendenza lineare delle variabili
plot(price ~ horsepower, data=df)

####################################

#3)CLUSTER ANALYSIS
library(rstudioapi)


library(stats)
library(Matrix)
library(pvclust)
library(mclust)
library(openxlsx)


# PROVA cluster con dataset ORIGINALE

#cluster con dataframe con tutte le variabili

summary(df)


sapply(df,sd)



rownames(df)=tesina[,3]


d0<-dist(df,method="canberra")

summary(d0)



dev.new()
dendro_ave <- hclust(d0,method='ward.D2')
plot(dendro_ave,cex=0.5)
gruppi=rect.hclust(dendro_ave, k=4)

rownames(df)=tesina[,3]
#cluster con dataframe con meno variabili

df1=data.frame(df[-c(3,4,6,7,8,9,11,13)])

View(df1)

d1<-dist(df1,method="canberra")

summary(d1)



dev.new()
dendro_ave1 <- hclust(d1,method='ward.D2')
plot(dendro_ave1,cex=0.5)
gruppi1=rect.hclust(dendro_ave1, k=4)


plot(dendro_ave1$height)

View(gruppi1[[1]])

dev.new()

np=length(tesina[,3])
plot(dendro_ave1, xlab=" ", ylab="Distanza ",main="Automobili",labels=FALSE,hang=-1, sub="L",
     xaxt="n", yaxt="n")
v=seq(from=1,to=(np-2), by=5)
axis(1,at=v, labels=tesina$CarName[dendro_ave1$order[v]],las=1,cex=0.1)
gruppi2=rect.hclust(dendro_ave,k=4)

aggregate(df1, by=list(cluster=dendro_ave1$cluster), mean)
aggregate(df1,by=list(dendro_ave1$cluster),FUN=mean, na.rm=TRUE)




####
rownames(df1)=tesina[,3]
g4<-cutree(dendro_ave1,k=4)

gruppo1=df1[which(g4==1),1:6]
## estraggo i nomi delle auto del primo gruppo
tesina$CarName[gruppo1]
dim(gruppo1)
gruppo2=df1[which(g4==2),1:6]
dim(gruppo2)
### ho estratto il dataset del gruppo 3
gruppo3=df1[which(g4==3),1:6]
dim(gruppo3)
gruppo4=df1[which(g4==4),1:6]
dim(gruppo4)

plot(dendro_ave1,xlab="AUTO",ylab="distanza canberra", labels=FALSE, hang=-1,sub="Legame Ward")
partizione=rect.hclust(dendro_ave1,k=4,cluster=g4)
taglia=cutree(dendro_ave1,4)


new_set=data.frame(df,as.factor(g4))
names(new_set)=c(names(df),"gruppo")
rownames(new_set)=df[,1]
View(new_set)

dim(new_set)

###grafici cluster

library(ggplot2)
library(factoextra)
library(cluster)
dev.new()
clusplot(new_set, new_set$gruppo, main='2D representation of the Cluster solution',
         color=TRUE, shade=FALSE,
         labels=2, cex.lab=0.05, lines=0)

#test pseudo F
anova_one_way <- aov(wheelbase~gruppo, data = new_set)
summary(anova_one_way)

anova_one_way1 <- aov(carlength~gruppo, data = new_set)
summary(anova_one_way1)

anova_one_way1 <- aov(carwidth~gruppo, data = new_set)
summary(anova_one_way1)

anova_one_way1 <- aov(citympg~gruppo, data = new_set)
summary(anova_one_way1)

anova_one_way1 <- aov(curbweight~gruppo, data = new_set)
summary(anova_one_way1)


boxplot(new_set$carlenght[new_set$gruppo==1],ylim=c(10,200))
par(new=TRUE)
boxplot(new_set$carlength[new_set$gruppo==2],ylim=c(10,200))
par(new=TRUE)
boxplot(new_set$carlenght[new_set$gruppo==3],ylim=c(10,200))



Comp=TukeyHSD(anova_one_way,ordered=TRUE,conf.level=0.95)
View(Comp$gruppo)
Diverse=subset(Comp$gruppo, Comp$gruppo[,4]<0.25)
View(Diverse)

# K-Means Cluster Analysis
fit <- kmeans(df1, 3, nstart=10000) # 3 cluster solution
print(fit)
print(fit$cluster[10]) # mi dice in quale gruppo si trova l'unit? 10
View(fit$cluster)
print(fit$cluster)
print(fit$centers)
nomi=names(ItPr)
## creo dataset con classificazione
aggregate(df1, by=list(cluster=fit$cluster), mean)
### Visualizza ciascuna variabile rispetto le altre
# get cluster means- calcola le medie dei gruppi
aggregate(df1,by=list(fit$cluster),FUN=mean, na.rm=TRUE)
# append cluster assignment
head(fit$cluster, 4)
library(ggplot2)
library(factoextra)
dev.new()
fviz_cluster(fit,df1)
View(df1)

#SILOHUETTE

library(cluster)

pr=pam(df1,3)

clusplot(pr)

# silouwtte quanto posso credere che quell unita sta veramente bene in quel gruppo

si=silhouette(pr)
dev.new()
plot(si)
str(si)
# valuta la misura di silhouette al variare del numero di gruppi
for(k in 2:6){
  dev.new()
  plot(silhouette(pam(df1, k=k)), main = paste("k = ",k), do.n.k=FALSE)
  mtext("PAM(df1) as in Kaufman & Rousseeuw, p.101",
        outer = TRUE, font = par("font.main"), cex = par("cex.main"));
  Sys.sleep(2)
}


##########################################################



#4)-----------------------------------------------------PCA
cov(df1)
A=cor(df1)
##### chiamo direttamente la matrice dei dati
## princomp usa la routine eigen
## prcomp usa la decomposizione ai valori singolari di X
pca_df1<-princomp(df1, cor=TRUE, scores=TRUE)
str(pca_df1)
pca_df1
print(summary(pca_df1),loading=TRUE)

pca_df1$scores
matplot(pca_df1$scores[,1],pca_df1$scores[,2], type='p', pch=8, xlab='non so x', ylab='non so y')
abline(h=0)
abline(v=0)

library(ggplot2)

# creo dataframe con le nuove variabile
disegno=data.frame(row.names(df1),pca_df1$scores[,1],pca_df1$scores[,2])
View(disegno)
ggplot(disegno,aes(disegno[,2],disegno[,3]))+geom_text(aes(label=row.names(df1)),size=4)
pca_df1_2=pca_df1$scores[,1:2]
#biplot
library(factoextra)
biplot(pca_df1, col=c("grey", "orange"), cex=0.5, xlab='non sox',ylab='non soy')
abline(h=0)
abline(v=0)

#biplot carino
fviz_pca_biplot(pca_df1, repel = TRUE,
                col.var = "#FF5733", # Variables color
                col.ind = "696969"  # Individuals color
                
)
## rappresentazione degli individui e i colori
## indicano individui simili

fviz_pca_ind(pca_df1,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
# Rappresentazione le variabili e le correlazione con le variabili
fviz_pca_var(pca_df1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

