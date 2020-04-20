```{r, echo = FALSE, message = FALSE}

matbinaire<-readRDS(file ="datamatrice/orange_matricebinaire.rds")
matnair<-readRDS(file ="datamatrice/orange_matnair.rds")

data<-list(c("datamatrice/DeGaulle_matricebinaire.rds","datamatrice/DeGaulle_matnair.rds"),c("datamatrice/orange_matricebinaire.rds","datamatrice/orange_matnair.rds"),c("datamatrice/painter_matricebinaire.rds","datamatrice/painter_matnair.rds"),c("datamatrice/scientist_matricebinaire.rds","datamatrice/scientist_matnair.rds"),c("datamatrice/apple_matricebinaire.rds","datamatrice/apple_matnair.rds"),c("datamatrice/presidents_matricebinaire.rds","datamatrice/presidents_matnair.rds"),c("datamatrice/Furniture_matricebinaire.rds","datamatrice/Furniture_matnair.rds"))

data_matrice<-list()

data_matrice$DeGaulle<-list(matricebinaire=readRDS(file = data[[c(1,1)]]),matnair=readRDS(file = data[[c(1,2)]]))

data_matrice$Orange<-list(matricebinaire=readRDS(file = data[[c(2,1)]]),matnair=readRDS(file = data[[c(2,2)]]))

data_matrice$Painter<-list(matricebinaire=readRDS(file = data[[c(3,1)]]),matnair=readRDS(file = data[[c(3,2)]]))

data_matrice$Scientist<-list(matricebinaire=readRDS(file = data[[c(4,1)]]),matnair=readRDS(file = data[[c(4,2)]]))

data_matrice$apple<-list(matricebinaire=readRDS(file = data[[c(5,1)]]),matnair=readRDS(file = data[[c(5,2)]]))

data_matrice$presidents<-list(matricebinaire=readRDS(file = data[[c(6,1)]]),matnair=readRDS(file = data[[c(6,2)]]))

data_matrice$Furniture<-list(matricebinaire=readRDS(file = data[[c(7,1)]]),matnair=readRDS(file = data[[c(7,2)]]))
```

# library(mclust)
# calcule la distance pour voir la matrice de distance

```{r, echo=TRUE, message=FALSE}

d<-dist(matnair, method="euclidian")
d<-data.matrix(d)
p<-ncol(d)
# install.packages("spam")
# library(spam)
# library(grid)
# library(fields)
cols=topo.colors(64)
image(1:p, 1:p, d, axes = FALSE, xlab="", ylab="",col=cols)
axis(1, 1:p, rownames(matnair), cex.axis = 0.5, las=3)
axis(2, 1:p, rownames(matnair), cex.axis = 0.5, las=1)
```


#Non-hierarchical clustering (NHC) 
##fonction kmeans :

```{r, echo=TRUE}
# library("cluster")
# library("vegan")

ustilisation_kmeans<-function(matrice, nbcluster)
{
  set.seed(1234)
  teen_clusters <- kmeans(matrice, centers =nbcluster)
  print(teen_clusters$cl)
 # matrice.KM.cascade<-cascadeKM(matrice, inf.gr=2, sup.gr=10, criterion="ssi")
 # plot(matrice.KM.cascade,sortg=TRUE)
 
 # plot(silhouette(teen_clusters$cl,matrice))
  return(teen_clusters)
}

```

##liste kmeans

```{r, echo=TRUE}

listekmeans<-list()
for(i in 1:length(data_matrice))
{
  listekmeans[[names(data_matrice)[i]]]$matricebinaire<-ustilisation_kmeans(data_matrice[[i]]$matricebinaire, data_matrice[[i]]$nb_cluster_matricebinaire)

  listekmeans[[i]]$matnair<-ustilisation_kmeans(data_matrice[[i]]$matnair,data_matrice[[i]]$nb_cluster_matnair)
}

```


#Classification hierarchique ascendante :

##Hclust
###fonction hclust :
```{r, echo=TRUE}
# library(devtools)
# install_github("larmarange/JLutils")
# library(JLutils)

ustilisation_hclust<-function(matrice, methode)
{
  d <- dist(matrice, method = methode)
  hc1 <- hclust(d, method = "ward.D2" )
  liste<-list()
  liste$hc<-hc1
  liste$distance<-d
  return(liste)
}

```

###liste de hclust
```{r, echo=TRUE}
colonne<-c("nb clustering", "distance")
liste<-list()
listehclustdistance<-list()
for(i in 1:length(data_matrice))
{
  fonctionbinary<-ustilisation_hclust(data_matrice[[i]]$matricebinaire,"binary")
  liste[[names(data_matrice)[i]]]$matricebinaire<-fonctionbinary$hc
  listehclustdistance[[names(data_matrice)[i]]]$matricebinaire<-fonctionbinary$distance
  
  fonctionnair<-ustilisation_hclust(data_matrice[[i]]$matnair,"euclidean")
  liste[[names(data_matrice)[i]]]$matnair<-fonctionnair$hc
  listehclustdistance[[names(data_matrice)[i]]]$matnair<-fonctionnair$distance
}

```

###tableau de hclust
```{r, echo=TRUE}
creationtableau<-function(data_matrice,liste)
{
  namerow<-c()
  nombredecluster<-c()
  distance<-c()
  nombredereque<-c()
  nombredeelementdansrequete<-c()
  # namecolonne<-c("nombreofthecluster","distance")
  for(i in 1:length(data_matrice))
  {
    n<-paste(names(data_matrice)[i],"matricebinaire",sep="+")
    m<-paste(names(data_matrice)[i],"matnair",sep="+")
    namerow<-c(namerow,n,m)
    distance<-c(distance,liste[[i]]$matricebinaire$dist.method,liste[[i]]$matnair$dist.method)
    nombredeelementdansrequete<-c(nombredeelementdansrequete, nrow(data_matrice[[i]][[1]]), nrow(data_matrice[[i]][[2]]))
    nombredecluster<-c(nombredecluster,data_matrice[[i]]$nb_cluster_matricebinaire,data_matrice[[i]]$nb_cluster_matnair)
  }
  data<-data.frame(distance,nombredeelementdansrequete,nombredecluster,row.names = namerow)
  return (data)
}
data_hclust<-creationtableau(data_matrice,liste)
# library(knitr)
knitr::kable(data_hclust)
```

###afficher un dendrogram

```{r, echo=TRUE}
library("cluster")
affichagedendrogram<-function(hc1, nbdegroups, names)
{
  plot(hc1,cex = 0.6,main = paste("Dendrogram of ",names))
  sub_grp <- cutree(hc1, k = nbdegroups)
  rect.hclust(hc1, k = nbdegroups, border = 2:5)
  return (sub_grp)
}

for(i in 1:length(data_matrice))
{

  sub_grp1<-affichagedendrogram(liste[[i]]$matricebinaire, data_matrice[[i]]$nb_cluster_matricebinaire, paste("hclust",names(liste[i]),names(liste[[i]])[1]))
  plot(silhouette(sub_grp1,listehclustdistance[[i]]$matricebinaire))
  print(names(sub_grp1))
  
  sub_grp2<-affichagedendrogram(liste[[i]]$matnair, data_matrice[[i]]$nb_cluster_matnair,paste("hclust",names(liste[i]),names(liste[[i]])[2]))
  plot(silhouette(sub_grp2,listehclustdistance[[i]]$matnair))
  print(names(sub_grp2))
}
```

### Agnes
### Fonction de agnes

```{r, echo=TRUE}
# library(cluster)
ustilisation_agnes<-function(matrice,nbcluster)
{
  hc <- agnes(matrice, method = "ward" )
  # pltree(hc,cex = 0.6, main = "Dendrogram of agnes")
  # sub_grp <-cutree(as.hclust(hc), k = nbcluster)
  # rect.hclust(hc, k = nbcluster, border = 2:5)
  return (hc)
}
```


### liste de agnes
```{r, echo=TRUE}
listeagnes<-list()
for(i in 1:length(data_matrice))
{
  listeagnes[[names(data_matrice)[i]]]$matricebinaire<-ustilisation_agnes(data_matrice[[i]]$matricebinaire,7)

  listeagnes[[i]]$matnair<-ustilisation_agnes(data_matrice[[i]]$matnair,7)
}

```
### tableau de agnes
```{r, echo=TRUE}
creationtableauagnes<-function(data_matrice,liste)
{
  namerow<-c()
  nombreofthecluster<-c()
  ac<-c()
  nombredecluster<-c()
  nombredeelementdansrequete<-c()
  # namecolonne<-c("nombreofthecluster","distance")
  for(i in 1:length(liste))
  {
    n<-paste(names(data_matrice)[i],"matricebinaire",sep="+")
    m<-paste(names(data_matrice)[i],"matnair",sep="+")
    namerow<-c(namerow,n,m)

    ac<-c(ac,liste[[i]]$matricebinaire$ac,liste[[i]]$matnair$ac)
    nombredeelementdansrequete<-c(nombredeelementdansrequete, nrow(data_matrice[[i]][[1]]), nrow(data_matrice[[i]][[2]]))

    nombredecluster<-c(nombredecluster,data_matrice[[i]]$nb_cluster_matricebinaire,data_matrice[[i]]$nb_cluster_matnair)

  }
  data<-data.frame(ac,nombredeelementdansrequete,nombredecluster,row.names = namerow)
  return (data)
}
data_agnes<-creationtableauagnes(data_matrice,listeagnes)

knitr::kable(data_agnes)

```
### dendrogram agnes
```{r, echo=TRUE}
affichagedendrogram_agnes<-function(hc1, nbdegroups, names)
{
  pltree(hc1,cex = 0.6,main = paste("Dendrogram of ",names))
  sub_grp <- cutree(hc1, k = nbdegroups)
  rect.hclust(hc1, k = nbdegroups, border = 2:5)
  return(sub_grp)
}

for(i in 1:length(data_matrice))
{
  sub_grp1<-affichagedendrogram_agnes(listeagnes[[i]]$matricebinaire, data_matrice[[i]]$nb_cluster_matricebinaire, paste("agnes",names(listeagnes[i]),names(listeagnes[[i]])[1]))
  plot(silhouette(sub_grp1,data_matrice[[i]]$matricebinaire))
  print(names(sub_grp1))
  
  sub_grp2<-affichagedendrogram_agnes(listeagnes[[i]]$matnair, data_matrice[[i]]$nb_cluster_matnair,paste("agnes",names(listeagnes[i]),names(listeagnes[[i]])[2]))
  plot(silhouette(sub_grp2,data_matrice[[i]]$matricebinaire))
  print(names(sub_grp2))
}

```




### diana
###fonction diana 

```{r, echo=TRUE}

# library("cluster")

ustilisation_diana<-function(matrice,nbcluster)
{
  hc <- diana(matrice)
  # print("Divise coefficient =")
  # print(hc$dc)
  # pltree(hc,cex = 0.6, main = "Dendrogram of diana")
  # sub_grp <-cutree(as.hclust(hc), k = nbcluster)
  # rect.hclust(hc, k = nbcluster, border = 2:5)
  return(hc)
}

```

###liste de diana
```{r, echo=TRUE}
listediana<-list()
for(i in 1:length(data_matrice))
{
  listediana[[names(data_matrice)[i]]]$matricebinaire<-ustilisation_diana(data_matrice[[i]]$matricebinaire,7)

  listediana[[i]]$matnair<-ustilisation_diana(data_matrice[[i]]$matnair,7)
}

```

###tableau de diana
```{r, echo=TRUE}
creationtableaudiana<-function(data_matrice,liste)
{
  namerow<-c()
  nombreofthecluster<-c()
  dc<-c()
  nombredecluster<-c()
  nombredeelementdansrequete<-c()
  # namecolonne<-c("nombreofthecluster","distance")
  for(i in 1:length(liste))
  {
    n<-paste(names(data_matrice)[i],"matricebinaire",sep="+")
    m<-paste(names(data_matrice)[i],"matnair",sep="+")
    namerow<-c(namerow,n,m)

    dc<-c(dc,liste[[i]]$matricebinaire$dc,liste[[i]]$matnair$dc)
    nombredeelementdansrequete<-c(nombredeelementdansrequete, nrow(data_matrice[[i]][[1]]), nrow(data_matrice[[i]][[2]]))

    nombredecluster<-c(nombredecluster,data_matrice[[i]]$nb_cluster_matricebinaire,data_matrice[[i]]$nb_cluster_matnair)

  }
  data<-data.frame(dc,nombredeelementdansrequete,nombredecluster,row.names = namerow)
  return (data)
}
data_diana<-creationtableaudiana(data_matrice,listediana)

knitr::kable(data_diana)

```
###dendrogram agnes
```{r, echo=TRUE}

for(i in 1:length(data_matrice))
{
  affichagedendrogram_agnes(listediana[[i]]$matricebinaire, data_matrice[[i]]$nb_cluster_matricebinaire, paste("diana",names(listediana[i]),names(listediana[[i]])[1]))
  affichagedendrogram_agnes(listediana[[i]]$matnair, data_matrice[[i]]$nb_cluster_matnair,paste("diana",names(listediana[i]),names(listediana[[i]])[2]))
}

```

#Classification Ascendantes Hierarchique with ACP (PCA + HCPC)

##fonction PCA_HCPC


```{r, echo=TRUE, message=FALSE}
# install.packages(c("FactoMineR", "factoextra"))
library("ggplot2")
library("FactoMineR")
# library("factoextra")
utilisation_PCA_HCPC<-function(matrice)
{
  #1. ACP
  matrice.pca<-PCA(matrice, graph = FALSE)

  #2. HCPC
  # On effectue une CAH sur les cooclrdonnees.
  # distance euidienne 
  matrice.hcpc<-HCPC(matrice.pca, nb.clust = -1,graph = FALSE) ##, metric ="manhattan"
 ")
  return(matrice.hcpc)
}

utilisation_PCA_HCPC(data_matrice[[2]]$matnair)
```



## List to HCPC with PCA

```{r, echo=TRUE}
listeHcpcPca<-list()
for(i in 1:length(data_matrice))
{
  listeHcpcPca[[names(data_matrice)[i]]]$matricebinaire<-utilisation_PCA_HCPC(data_matrice[[i]]$matricebinaire)

  listeHcpcPca[[i]]$matnair<-utilisation_PCA_HCPC(data_matrice[[i]]$matnair)
}

```

### dendrogram Hcpc with PCA
```{r, echo=TRUE}
affichagedendrogram_hcpc_pca<-function(hc, names)
{
  plot(hc, choice ="tree", main = paste("Dendrogram of hcpcwithPCA",names))
}

for(i in 1:length(data_matrice))
{
  affichagedendrogram_hcpc_pca(listeHcpcPca[[i]]$matricebinaire, paste(names(listeHcpcPca[i]),names(listeHcpcPca[[i]])[1]))
  
  affichagedendrogram_hcpc_pca(listeHcpcPca[[i]]$matnair, paste(names(listeHcpcPca[i]),names(listeHcpcPca[[i]])[2]))
}

```

