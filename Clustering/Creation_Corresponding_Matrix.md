### 1. Search neighbor
#install.packages("stringr", dependencies=TRUE)

```
library(stringr)
library(SPARQL)

rechercheDesvoisins<-function(donnee)
{
  endpoint <- "http://dbpedia.org/sparql"
  query <-  paste("SELECT ?b ?c
                  WHERE{
                  {",donnee,"?b ?c . FILTER ( !( regex (?b, 'abstract', 'i') || regex ( ?b, 'comment', 'i')))
                  FILTER isIRI (?c)
                  } UNION
                  {",donnee,"?b ?c . FILTER ( !( regex (?b , 'abstract', 'i') || regex ( ?b, 'comment', 'i') ))
                  FILTER isLiteral(?c)
                  FILTER ( lang(?c) = 'en')
                  }
                  }")
  qd2 <- SPARQL(endpoint,query)
  df2 <- qd2$results
  return(df2)
}
```
### 2. Look for the vector which will allow to have the name of all the columns of the future matrix ----

### 2.a.we concatenate the data of the neighbor of each response of the request ie ?b ?c
```

fusionDesDonnees<-function(matrice)
{
  matriceFusion<-matrix(NA, nrow=nrow(matrice))
  for(i in 1:nrow(matrice))
  {
    matriceFusion[i]<-paste(matrice[i,1],"|",matrice[i,2])
  }
  return(matriceFusion)
}

##2.b. We add the line number of the response to the SPARQL request here df2 (step 0)

ajouterlareponse<-function(test,i)
{
  for(j in 1:length(test))
  {
    test[j]<-paste(i,"|",test[j])
    
  }
  return(test)
}
```
### 2.c.We recover the responses of all the requests and we create a vector with all the type responsesi | ?b ou i | ?b | ?c 
### Remove duplicates of i || ?b 
```
vecteurDesSousVoisin<-function(df2)
{
  vecteur=c()
  for(i in 1:nrow(df2))  ##pour toute les requetes 
  {
    sousvoisin<-rechercheDesvoisins(df2[[1]][i])
    test<-sousvoisin$b
    # test<-table(factor(test))
    # test<-names(test)
    doublepourb<-ajouterlareponse(test,i)
    
    autrepartie<-fusionDesDonnees(sousvoisin)
    autrepartieaveclenomdelarequete<-ajouterlareponse(autrepartie,i)
    
    
    vecteur<-c(vecteur,doublepourb,autrepartieaveclenomdelarequete)
  }
  return(vecteur)
}

```
#Create a vector with all the columns of the matrix 

### 3. Function to see if i | ? b or i | ? b | ? c is in the neighborhood of each sparql reuqete (step 0) ----
### We create a function that takes as argument all the responses, column name and number line
### We return the name of ?b or ?b| ?c if there is a match with the neighborhood of each sparql request
```
estdanslevoisinage <-function(veceteurToutLesVoisinsPossibles, nomcolone, numeroligne)
{
  retur=NA
  separe = strsplit(nomcolone,split='|', fixed=TRUE)
  if(length(separe[[1]])==2)
  {
    if(paste(numeroligne, ' |',separe[[1]][2],sep = "")%in% veceteurToutLesVoisinsPossibles)
    {
      retur=separe[[1]][2]
    }
  }
  if(length(separe[[1]])==3) 
  {
    if(paste(numeroligne, ' |',separe[[1]][2],'|',separe[[1]][3],sep="")%in% veceteurToutLesVoisinsPossibles)
    {
      retur=paste(separe[[1]][2],'|',separe[[1]][3])
    }
  }
  return(retur)
}
```
### 4. Create the correspondence matrix  ----
```
creationdelamatrice<-function(df2,Possibles)
{
  
  matrice<-matrix(NA,nrow = nrow(df2), ncol = length(Possibles))
  colnames(matrice)<-Possibles
  for(j in 1:ncol(matrice))
  {
    for(i in 1:nrow(matrice))
    {
      matrice[i,j]=estdanslevoisinage(Possibles,colnames(matrice)[j],i)
    }
  }
  rownames(matrice)<-df2$y
  return (matrice)
}
```
