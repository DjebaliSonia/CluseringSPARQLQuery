```{r setup, include=FALSE}
estdanslamatrice<-function(matrice, ligne, colonne)
{
  reponse=0
  if(is.na(matrice[ligne,colonne]))
  {
    reponse=0
  }
  else
  {
    reponse =1
  }
  return (reponse)
}
```
```
nomcolonne<-function(matrice, colonne)
{
  for(i in 1:nrow(matrice))
  {
    if(is.na(matrice[i,colonne])==FALSE)
    {
      m=matrice[i,colonne]
    }
  }
  return(m)
}
```
```
matricebinaire<-function(matrice)
{
  matbinaire=matrix(NA, nrow = nrow(matrice), ncol = ncol(matrice))
  rownames(matbinaire)<-rownames(matrice)
  
  vecteur= vector()
  for(j in 1:ncol(matbinaire))
  {
    for(i in 1:nrow(matbinaire))
    {
      matbinaire[i,j]=estdanslamatrice(matrice,i,j)
      
    }
    vecteur=c(vecteur,nomcolonne(matrice,j))
  }
  colnames(matbinaire)<-colnames(matrice)
  return (matbinaire)
}
```
