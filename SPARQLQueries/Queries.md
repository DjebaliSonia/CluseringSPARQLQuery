# install.packages("SPARQL")
library(SPARQL)

endpoint <- "http://dbpedia.org/sparql"
  
query1 <-  
"SELECT distinct ?g ?y
  WHERE {
        ?g rdfs:label ?y
         FILTER(regex(?y, 'De Gaulle', 'i'))
         } LIMIT 10"
qd1 <- SPARQL(endpoint,query1)
DeGaulle <- qd1$results


query2 <-  "SELECT ?g ?y
  WHERE {
        <http://dbpedia.org/resource/Orange> <http://dbpedia.org/ontology/wikiPageDisambiguates> ?g .
         ?g rdfs:label ?y .
        FILTER(lang(?y)=\"fr\")
        }"
qd2 <- SPARQL(endpoint,query2)
Orange <- qd2$results



query3 <-  "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  SELECT ?person ?y
  WHERE {    
        ?person rdf:type <http://dbpedia.org/ontology/Scientist>.
        ?person dbo:birthDate ?date.
        ?person rdfs:label ?y
        FILTER (?date > \"1890-01-1\"^^xsd:date && ?date < \"1900-01-1\"^^xsd:date)
        FILTER(lang(?y)=\"fr\")
        }LIMIT 10"
qd3 <- SPARQL(endpoint,query3)
scientist <- qd3$results



query4 <-"PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
   SELECT ?person ?y
   WHERE {    
          ?person rdf:type dbo:Painter.
          ?person dbo:birthDate ?date.
          ?person rdfs:label ?y
          FILTER (?date > \"1800-01-1\"^^xsd:date && ?date < \"1900-01-1\"^^xsd:date)
          FILTER(lang(?y)=\"fr\")
          }LIMIT 10"
qd4 <- SPARQL(endpoint,query4)
painter <- qd4$results

query5 <-"SELECT ?resource ?y
   WHERE {
          ?resource rdfs:label ?y.
          FILTER( regex(?y, \"Apple\", \"i\") )
          }LIMIT 10"
qd5 <- SPARQL(endpoint,query5)
apple <- qd5$results

query6<-"SELECT ?x ?y
  WHERE {
        ?x rdf:type  <http://dbpedia.org/class/yago/WikicatDemocraticPartyPresidentsOfTheUnitedStates>.
        ?x rdfs:label ?y
       FILTER(lang(?y)=\"en\")
}"
qd6 <- SPARQL(endpoint,query6)
presidents <- qd6$results


query7<-"SELECT ?x ?y
  WHERE {
         ?x dbp:subdivisionType <http://dbpedia.org/resource/Regions_of_France>.
         ?x rdfs:label ?y.
          FILTER(lang(?y)=\"fr\")
        }LIMIT 10"
qd7 <- SPARQL(endpoint,query7)
regions <- qd7$results

query8<-"Select distinct ?person ?y
  WHERE {
        ?person dct:subject dbc:British_feminists.
        ?person rdfs:label ?y
        FILTER(lang(?y)=\"en\")
      }LIMIT 10"
qd8<-SPARQL(endpoint,query8)
feminists <-qd8$results

query9 <-"Select distinct ?x ?y
  WHERE {
      ?x dct:subject dbc:Furniture .
      ?x rdfs:label ?y
      FILTER(lang(?y)=\"en\")
      }LIMIT 10"
qd9<-SPARQL(endpoint,query9)
Furniture <-qd9$results

query10<-"PREFIX : <http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#> 
  SELECT DISTINCT * WHERE
   { 
    ?z a :Department . 
    ?z :subOrganizationOf ?y . 
    ?y a :University . 
    ?x :undergraduateDegreeFrom ?y . 
    ?x a :GraduateStudent . 
    ?x :memberOf ?z . 
    }"
qd10<-SPARQL(endpoint,query10)
qd10result <-qd10$results


file1 = "data/DeGaulle.rds"
saveRDS(DeGaulle, file1)

file2 = "data/Orange.rds"
saveRDS(Orange, file2)

file3 = "data/scientist.rds"
saveRDS(scientist, file3)

file4 = "data/painter.rds"
saveRDS(painter, file4)

file5 = "data/apple.rds"
saveRDS(apple, file5)

file6="data/presidents.rds"
saveRDS(presidents, file6)

file7="data/regions.rds"
saveRDS(regions, file7)

file8="data/feminists.rds"
saveRDS(feminists, file8)

file9="data/Furniture.rds"
saveRDS(Furniture, file9)


adress<-c(file1,file2,file3,file4,file5,file6,file7,file8,file9)
saveRDS(adress,file="data/adress.rds")




