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

```

	
### Creation_Corresponding_Matrix
```{r pressure, echo=FALSE}
source("02_creation_matrice_correspondance_ammelioration.R")
doublonstest<-which(duplicated(vecteur))
vecteursansdouble<-vecteur[-doublonstest]
```

### BinaryMatrix
```{r pressure, echo=FALSE}
source("03_matricebinaire.R")
colnames(matbinaire)<-vecteursansdouble

```


```{r pressure, echo=FALSE}

library("clValid")
intern <- clValid(matbinaire, nClust = 2:9, metric = "manhattan",
              clMethods = c("hierarchical","agnes","diana"), validation = "internal")

summary(intern)
optimalScores(intern)
plot(intern)

```


### Optimal number of clusters 
```{r pressure, echo=FALSE}
library(fpc)
set.seed(20000)
pk<-pamk(matbinaire, metric="manhattan", krange=2:9, criterion="ch", usepam=TRUE)
nombreoptimal=1

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

### Clustering methods
		
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


### DIANA : DIvisie ANAlysis clustering algorithm
```{r pressure, echo=FALSE}

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
  

```

### AGNES : GNES (Agglomerative Nesting)
```{r pressure, echo=FALSE}

  hc3 <- agnes(matbinaire, method = "ward", metric="manhattan" )
  pltree(hc3, cex = 0.6, main = "Dendrogram of agnes")
  sub_grp3 <-cutree(as.hclust(hc3), k = nombreoptimal)
  rect.hclust(hc, k = nombreoptimal, border = 2:5)
  print(hc3$ac)
  ss3<-silhouette(sub_grp3,matbinaire)
  plot(ss3)


```


### HCPC with PCA
````{r pressure, echo=FALSE}
# install.packages(c("FactoMineR", "factoextra"))
library("ggplot2")
library("FactoMineR")



### 1. ACP
matricepca<-CA(matbinaire, graph = FALSE)


#""" 2. HCPC
hcpc4<-HCPC(matricepca, metric="manhattan", nb.clust = -1,graph=TRUE )

```


