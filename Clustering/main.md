---
title: "resumee3"
output: html_document
editor_options: 
  chunk_output_type: console
---

Entrer une requete SPARQL 
```{r setup, include=FALSE}
# install.packages("SPARQL")


library(SPARQL)
limite = 30
fairerequete<-function(limite)
{
endpoint <- "http://dbpedia.org/sparql"
query <- paste( "SELECT ?resource ?y
WHERE {
  ?resource rdfs:label ?y.
  FILTER( regex(?y, \"Apple\", \"i\") )
}LIMIT",limite,"offset 10")


  qd <- SPARQL(endpoint,query)
  Resultquery <- qd$results
  return(Resultquery)
}
# Rprof("tempsResult", append=TRUE)
# 
# vv<-fairerequete(10)
# Rprof(NULL)
# #library(proftools)
# flatProfile(readProfileData("tempsResult.out"))
# summaryRprof("tempsResult.out")

temps<-system.time(Resultquery<-fairerequete(limite))
print(temps)
```


matrice de correspondance
```{r pressure, echo=FALSE}
source("02_creation_matrice_correspondance_ammelioration.R")
temps1<-system.time(vecteur<-vecteurDesSousVoisin(Resultquery))
doublonstest<-which(duplicated(vecteur))
 vecteursansdouble<-vecteur[-doublonstest]
temps2<-system.time(matrice<-creationdelamatrice(Resultquery,vecteur))
print(temps1+temps2)
```

matrice binaire
```{r pressure, echo=FALSE}
source("03_matricebinaire.R")
temps3<-system.time(matbinaire<-matricebinaire(matrice,vecteur))
colnames(matbinaire)<-vecteursansdouble
print(temps3)
# matbinairesansdoublons<-matbinaire
# doublonstest<-which(duplicated(vecteur))
# #doublonstest
# test2<-colnames(matbinairesansdoublons)[-doublonstest]
# matbinairesansdoublons<- matbinairesansdoublons[,test2]

```



```{r pressure, echo=FALSE}

library("clValid")
intern <- clValid(matbinaire, nClust = 2:9, metric = "manhattan",
              clMethods = c("hierarchical","agnes","diana"), validation = "internal")

summary(intern)
optimalScores(intern)
plot(intern)

```


nombre de cluster optimal
```{r pressure, echo=FALSE}
library(fpc)
set.seed(20000)
pk<-pamk(matbinaire, metric="manhattan", krange=2:9, criterion="ch", usepam=TRUE)
nombreoptimal=1


# library(NbClust)
# res<-NbClust(matbinairesansdoublons, distance = "binary", min.nc=2, max.nc=6, method = "complete", index = "silhouette")
# res$All.indexnom
# res$Best.nc
# res$Best.partition


vecteur=round(pk$crit, digits=1)
max=1
for(i in 1:length(vecteur))
{
  if(vecteur[i]>=vecteur[max])
  {
    max=i
  }
}
nombreoptimal=max
print(nombreoptimal)
```

##Methode de hclust  
```{r pressure, echo=FALSE}
ptm <- proc.time()
d <- dist(matbinaire, method = "binary")
hc1 <- hclust(d, method = "ward.D2" )

plot(hc1,cex = 0.6,main = paste("Dendrogram hclust"))
sub_grp <- cutree(hc1, k = nombreoptimal)
rect.hclust(hc1, k = nombreoptimal, border = 2:5)
# library("cluster")
####################################

ss<-silhouette(sub_grp,d)
plot(ss, main = "Silhouette of hclust")
print(names(sub_grp))


####################################
dd<-dunn(d, sub_grp)
proc.time() - ptm

fviz_dend(hc1)   # Augment the room for labels)

```


##diana
```{r pressure, echo=FALSE}
ptm1 <- proc.time()
  hc <- diana(matbinaire,metric = "manhattan")
  print("Divise coefficient =")
  print(hc$dc)
  pltree(hc,cex = 0.6, main = "Dendrogram of diana")
  sub_grp2 <-cutree(as.hclust(hc), k = nombreoptimal)
  rect.hclust(hc, k = nombreoptimal, border = 2:5)
 
  
  
  d1 <- dist(matbinaire, method = "manhattan")
  ss1<-silhouette(sub_grp2,d1)
  plot(ss1, main = "Silhouette of hclust")
  print(names(sub_grp2))
  
proc.time() - ptm1

```

##agnes
```{r pressure, echo=FALSE}
ptm3 <- proc.time()
  hc3 <- agnes(matbinaire, method = "ward", metric="manhattan" )
  pltree(hc3, cex = 0.6, main = "Dendrogram of agnes")
  sub_grp3 <-cutree(as.hclust(hc3), k = nombreoptimal)
  rect.hclust(hc, k = nombreoptimal, border = 2:5)
  print(hc3$ac)
  ss3<-silhouette(sub_grp3,matbinaire)
  plot(ss3)
proc.time() - ptm3

```


##Methode de PCA_HCPC
````{r pressure, echo=FALSE}
# install.packages(c("FactoMineR", "factoextra"))
library("ggplot2")
library("FactoMineR")

ptm4 <- proc.time()

#1. ACP
matricepca<-CA(matbinaire, graph = FALSE)
print(proc.time() - ptm4)

ptm5 <- proc.time()

#2. HCPC
# On effectue une CAH sur les cooclrdonnees.
# distance euidienne
hcpc4<-HCPC(matricepca, metric="manhattan", nb.clust = -1,graph=TRUE )
#fviz_dend(matrice.hcpc4)
#plot(matrice.hcpc4, choice ="tree")
# plot(matrice.hcpc, choice ="tree", main = paste("Dendrogram of hcpcwithPCA"))

#km_stats <- cluster.stats(dd, res.hcpc$data.clust$clust)


#dd<-dist(matbinaire, method="manhattan")
#ss<-silhouette(hcpc4,dd)
#plot(ss, main = "Silhouette of hclust")




print(proc.time() - ptm5)

```




<!-- ##Methode de hclust ameliorer avec une suppression des élements dont la silhouette est inferieur à 0.25*(moyenne de la silhouette) -->
<!-- ```{r pressure, echo=FALSE} -->
<!-- retourner<-function(mat, silouette,nb) -->
<!-- { -->
<!--   nouveaumatrice<-matrix(NA,ncol = ncol(mat)) -->
<!--   # matriceDemalPlacer<-matrix(NA, ncol = ncol(mat)) -->
<!--   nom=c() -->
<!--   nommaplacer=c() -->
<!--   for(i in (1:nrow(mat))) -->
<!--   { -->
<!--     if(silouette[i]>=nb) -->
<!--     { -->
<!--       nouveaumatrice=rbind(nouveaumatrice,mat[i,]) -->
<!--       nom=c(nom,rownames(mat)[i]) -->
<!--     } -->
<!--     else -->
<!--     { -->
<!--       #matriceDemalPlacer=rbind(matriceDemalPlacer,mat[i,]) -->
<!--       nommaplacer=c(nommaplacer,rownames(mat)[i]) -->
<!--     } -->
<!--   } -->
<!--   nouveaumatrice=nouveaumatrice[-1,] -->
<!--   rownames(nouveaumatrice)<-nom -->
<!--   #rownames(matriceDemalPlacer)<-nommaplacer -->
<!--   liste<-list() -->
<!--   liste$nouvellematrice<-nouveaumatrice -->
<!--   liste$matriceDemalPlacer<-nommaplacer -->
<!--   return(liste) -->
<!-- } -->

<!-- nouvellemat=matrix() -->
<!-- vecteur=ss[,3] -->
<!-- liste<-list() -->
<!-- liste<-retourner(matbinairesansdoublons, vecteur,0.1225) -->
<!-- nouvellemat<-liste$nouvellematrice -->
<!-- dnouvellemat <- dist(liste$nouvellematrice, method = "binary") -->
<!-- hcnouvellemat <- hclust(dnouvellemat, method = "ward.D2" ) -->


<!-- names="orange wihout 0.25*0.2" -->
<!-- plot(hcnouvellemat,cex = 0.6,main = "Dendrogram of hclust optimal") -->
<!-- sub_grpnew <- cutree(hcnouvellemat, k = nombreoptimal) -->
<!-- rect.hclust(hcnouvellemat, k = nombreoptimal, border = 2:5) -->
<!-- library("cluster") -->
<!-- ssnew<-silhouette(sub_grpnew,dnouvellemat) -->
<!-- #rownames(ssnew)<-names(sub_grpnew) -->
<!-- plot(ssnew,main = "Silhouette of hclust optimal" ) -->
<!-- print(names(sub_grpnew)) -->
<!-- ``` -->

<!-- ```{r pressure, echo=FALSE} -->
<!-- matricemalplacer<-matrix(NA,nrow=length(liste$matriceDemalPlacer)) -->
<!-- matricemalplacer<-matbinairesansdoublons[liste$matriceDemalPlacer,] -->
<!-- print("Mal placer :") -->
<!-- print(rownames(matricemalplacer)) -->

<!-- dmalplacer <- dist(matricemalplacer, method = "binary") -->
<!-- hcmalplacer <- hclust(dmalplacer, method = "ward.D2" ) -->

<!-- library(fpc) -->
<!-- set.seed(20000) -->
<!-- pkmalplacer<-pamk(matbinairesansdoublons, krange=2:9, criterion="asw",critout=TRUE, usepam=TRUE) -->
<!-- nombreoptimalmalplacer= 3 -->

<!-- plot(hcmalplacer,cex = 0.6,main = "Dendrogram of hclust optimal") -->
<!-- sub_grpmalplacer <- cutree(hcmalplacer, k =nombreoptimalmalplacer ) -->
<!-- rect.hclust(hcmalplacer, k = nombreoptimalmalplacer, border = 2:5) -->
<!-- library("cluster") -->
<!-- ssmalplacer<-silhouette(sub_grpmalplacer,dmalplacer) -->
<!-- #rownames(ssnew)<-names(sub_grpnew) -->
<!-- plot(ssmalplacer,main = "Silhouette of hclust optimal" ) -->
<!-- print(names(sub_grpmalplacer)) -->

<!-- ``` -->
