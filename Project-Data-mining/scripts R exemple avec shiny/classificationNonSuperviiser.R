
# Pour la classification non superviser avec hcust

#Chargement du fichier
fromage<- read.table("D:\\M1\\Projet TSOPZE avec Camille\\fromage.txt",header = T,row.names = 1,sep = " ",dec = ".")

#Graphique croisement deux a deux
pairs(fromage)

#Centrage et reduction des donnees
#pour eviter que les variables a forte varience present indument sur les resultat
fromage.cr<-scale(fromage,center = T,scale = T)

#matrice des distances entre individus
d.fromage<-dist(fromage.cr)

#CAH - critère de Ward
cah.ward<-hclust(d.fromage,method = "ward.D2")

#affichage dendrogramme
plot(cah.ward)

#dendrogramme avec matérialisation des groupes
rect.hclust(cah.ward,k=4)

#découpage en 4 groupes
groupes.cah <- cutree(cah.ward,k=4)

#liste des groupes
print(sort(groupes.cah))

#Classification avec K-means

#k-means avec les données centrées et réduites
#center = 4 - nombre de groupes demandés
#nstart = 5 - nombre d'essais avec différents individus de départ
#parce que les résultats sont dépendants de l’initialisation
groupes.kmeans <- kmeans(fromage.cr,centers=4,nstart=5)

#affichage des résultats
print(groupes.kmeans)

#correspondance avec les groupes de la CAH
print(table(groupes.cah,groupes.kmeans$cluster))

#(1)évaluer la proportion d'inertie expliquée
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(fromage.cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}
#graphique
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
#(2) indice de Calinski Harabasz - utilisation du package fpc
library(fpc)
#évaluation des solutions
sol.kmeans <- kmeansruns(fromage.cr,krange=2:10,criterion="ch")
#graphique
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")

#fonction de calcul des stats
stat.comp <- function(x,y){
  #nombre de groupes
  K <- length(unique(y))
  #nb. d'observations
  n <- length(x)
  #moyenne globale
  m <- mean(x)
  #variabilité totale
  TSS <- sum((x-m)^2)
  #effectifs conditionnels
  nk <- table(y)
  #moyennes conditionnelles
  mk <- tapply(x,y,mean)
  #variabilité expliquée
  BSS <- sum(nk * (mk - m)^2)
  #moyennes + prop. variance expliquée
  result <- c(mk,100.0*BSS/TSS)
  #nommer les élements du vecteur
  names(result) <- c(paste("G",1:K),"% epl.")
  #renvoyer le vecteur résultat
  return(result)
}
#appliquer stat.comp aux variables de la base originelle fromage
#et non pas aux variables centrées et réduites
print(sapply(fromage,stat.comp,y=groupes.cah))

#ACP normée
acp <- princomp(fromage,cor=T,scores=T)
#screeplot - 2 axes retenus
plot(1:9,acp$sdev^2,type="b",xlab="Nb. de facteurs",ylab="Val. Propres")
#biplot
biplot(acp,cex=0.65)

#positionnement des groupes dans le plan factoriel avec étiquettes des points
plot(acp$scores[,1],acp$scores[,2],type="n",xlim=c(-5,5),ylim=c(-5,5))
text(acp$scores[,1],acp$scores[,2],col=c("red","green","blue","black")[groupes.cah],cex
     =0.65,labels=rownames(fromage),xlim=c(-5,5),ylim=c(-5,5))

