```
possede3parie <-function(nomcolone)
{
  retur=FALSE
  separe = strsplit(nomcolone,split="|", fixed=TRUE)
  if((length(separe[[1]])==3))
  {
    retur=TRUE
  }
  return(retur)
}
```
```
separer_sans_le_numero<-function(nomcolone)
{
    separe = strsplit(nomcolone,split=' |', fixed=TRUE)
    return(separe[[1]][2])
}

```
```
est_dans_la_ligne<-function(matrice, ligne, colonne)
{
  est=TRUE
  if(matrice[ligne,colonne]==0)
  {
    est=FALSE
  }
  return(est)
}
```
```
matricepreparation<-function(matbinaire)
{
  v=c()
  nouveaumatrice<-matrix(0,nrow =nrow(matbinaire))
  rownames(nouveaumatrice)<-rownames(matbinaire)
  for (i in 1:ncol(matbinaire))
  {
    if(possede3parie(colnames(matbinaire)[i])==TRUE)
    {
      v=c(v,colnames(matbinaire)[i])
      nouveaumatrice<-cbind(nouveaumatrice,matbinaire[,i])
    }
  }
  nouveaumatrice<-nouveaumatrice[,-1]
  colnames(nouveaumatrice)<-v
  return(nouveaumatrice)
}
```
```
attributunnombre<-function(nomcolonne,vecteur,nomvecteur,nombreacactuelle)
{
  nb=0
  v<-separer_sans_le_numero(nomcolonne)
  if(v %in% nomvecteur)
  {
    nb=nombreacactuelle+vecteur[v]
  }
  else
  {
    nb=nombreacactuelle
  }
  return (nb)
}

```
```
matricenair<-function(matbinaire)
{
  nouveaumatrice<-matricepreparation(matbinaire)
  voisin<-colnames(matbinaire)
  
  liste<-list()
  
  
  for(j in 1:nrow(matbinaire))
  {
    v=c()
    compt=0
    for (i in 1: ncol(matbinaire))
    {
      
      if(possede3parie(colnames(matbinaire)[i])==FALSE && est_dans_la_ligne(matbinaire,j,i)==TRUE)
      {
        v=c(v,separer_sans_le_numero(colnames(matbinaire)[i]))
      }
    }
    compt<-table(factor(v))
    liste[[j]]<-compt
  }
 
  for(j in 1:nrow(nouveaumatrice))
  {
    vecteur<-liste[[j]]
    nomvecteur<-names(vecteur)
    for(i in 1:ncol(nouveaumatrice))
    {
      nb<-nouveaumatrice[j,i]
      nouveaumatrice[j,i]=attributunnombre(colnames(nouveaumatrice)[i],vecteur,nomvecteur,nb)
    }
  }
   
  return(nouveaumatrice)
}
```
















