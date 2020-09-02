#permet de creer une transaction a partir d'une liste 
 trans <- as(a_list, "transactions")

#supp correspond au support, plus on diminue le supoort , plus les regles augmente
rule=apriori(trans,parameter=list(supp=0.3,conf=0.8,target="rules"))

#permet d'affciher le nombre de regle
rule

#permet d'affiher les regles obtenus 
inspect(rule[1:19])

#formule permettant de calculer le lift#######################
#pour extraire regles avec iris, il faut discretiser les 4 premiers colonnes
#pour discretiser,on peut utiliser la fonction cut ou la fonction categorize
#breaks, correspond au nombres d'intervalles qu'on peut avoir, 
#lables, coorespond au nom des intervalles 
iris_new[,4]<-cut(iris_new[,4], breaks=3,labels=c("petit","moyen","grand")) 

#permet d'effectuer l'algorithme appriori sur la transaction 
#la fonction appriori a comme autre parametre apparence, qui permet de re
rule=apriori(trans_iris,parameter=list(supp=0.3,conf=0.8,target="frequent itemsets"))
#lorsqu'on discretise les donnees, les attributes deveiennent des colonnes 
frequent_items=apriori(trans_iris,parameter=list(supp=0.3,conf=0.8,target="frequent itemsets"),appearance = list(rhs = c("Species=virginica"), default="lhs"))
