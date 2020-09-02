#Connexion + accès à la BD
##adapter cette partie au package que vous utilisez
library(RODBC)
conn = odbcConnect("info", uid = "root", pwd = "")
query="select Num_piece,Designation from toto_vente;"
BD=sqlQuery(conn, query)

#### Il faut sélectionner toute la suite et exécuter avec CRTL+R

i=1
n=nrow(BD)  ###nombre de lignes
m=length(levels(BD[,"designation"]))## nombre d'éléments différents
v1=rep(0,m)
names(v1)= levels(BD[,"designation"]) ##On utilise les noms des éléments coe nom de colonnes
v1[BD[i,"designation"]]=1
while(BD[i,"Num_piece"]==BD[i+1,"Num_piece"]&&i<n)
	{
	v1[BD[i+1,"designation"]]=1
	i=i+1
	}
M=matrix(v1,ncol=m,byrow=T) 

while(i<=n)
{
i=i+1
v1=rep(0,m)
names(v1)= levels(BD[,"designation"])
v1[BD[i,"designation"]]=1
	while(BD[i,"Num_piece"]==BD[i+1,"Num_piece"]&&i<n)
	{
	v1[BD[i+1,"designation"]]=1
	i=i+1
	}
if(i<=n){M=rbind(M,v1)}
}

rownames(M)=levels(BD[,"Num_piece"])

library(arules)
tr <- as(M, "transactions")

## Vous pouvez continuer avec l'extraction des règles telle que vue en cours.



