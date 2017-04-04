#Objectif: proposer une autre station de départ (respectivement d'arrivée) proche
#de la station de départ de l'utilisateur (respectivement d'arrivée) si celle-ci est vide (pleine) 

#critère de proximité: 250m
#critère de décision "station pleine/vide": nb de vélos prédits proche des bornes à 2 vélos près (ie 2 vélos ou bikesMax-2)

library(XML)
library(RCurl)
library(methods)
library(fasttime)
library(caret)


concatenationStringFile <- function(w1,w2){
  x<- c(w1,w2)
  y<- paste(x,sep="",collapse="/")
}

concatenationString <- function(w1,w2){
  x<- c(w1,w2)
  y<- paste(x,sep="",collapse="")
}

path="/Users/matthieurenard/Documents/X/PSC/data/paris/coordinates.csv"

coordonnées=read.csv(path)
stations=coordonnées$station  #liste de string des noms de stations
l=length(stations)


prediction_bikes <- function(model, heure.normalisee, jour.semaine.normalise, description.grade, temperature.grade, wind.grade,is.weekday,qualitytime_1, qualitytime_2, qualitytime_3, bikes_1, bikes_2, bikes_3, bikes_before, timediff){ #on suppose qu'on a calculé avant toutes les données
  data=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/data_neighbors",station),'.csv'))
  fitControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10) 
  Sys.setenv(TZ='GMT')
  data$moment=fastPOSIXct(data$moment,tz="GMT")
  n=length(data$bikes)

  #sélection du bon modèle
  data_train<-data[2:floor(8*n/10),]
  model <- train(bikes ~ heure.normalisee+jour.semaine.normalise+description.grade+temperature.grade+wind.grade+is.weekday+qualitytime_1+qualitytime_2+qualitytime_3+bikes_1+bikes_2+bikes_3, data_train, na.action=na.omit, method = "gbm", trControl = fitControl,verbose = FALSE)
#+bikes_before+timediff
  #requete pour la prédiction
  requete=data.frame(heure.normalisee=heure.normalisee, jour.semaine.normalise=jour.semaine.normalise, description.grade=description.grade, temperature.grade=temperature.grade, wind.grade=wind.grade,is.weekday=is.weekday,qualitytime_1=qualitytime_1, qualitytime_2=qualitytime_2, qualitytime_3=qualitytime_3, bikes_1=bikes_1,bikes_2=bikes_2,bikes_3=bikes_3,bikes_before=bikes_before,timediff=timediff)
  bikes=predict(model,requete)

  return(bikes)
  }

prediction_bikes('00906-gare-de-lest', 0.611828703703704,0.666666666666667,0.85,0.293314763231198,0.252427184466019,1,54,2.48333333333333,2.01666666666667,1,3,0, 0, 0)

#Etape 1: collecte des distances entre deux stations via l'API Google Maps et sélection des stations proches (fichiers de valentin)

latitude<- function(station){ #station est un string
  fichier=read.table(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/modified_data/paris/stations",concatenationString(station,".txt")), header=FALSE)
  return(fichier$V3)
}

longitude <- function(station){
  fichier=read.table(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/modified_data/paris/stations",concatenationString(station,".txt")), header=FALSE)
  return(fichier$V7)
}

distance <- function(station1, station2){  #distance entre station 1 et 2 en km, station 1 et 2 sont des strings, renvoie un int
  requete=getURL(concatenationString("https://maps.googleapis.com/maps/api/distancematrix/xml?origins=",concatenationString(latitude(station1),concatenationString(",",concatenationString(longitude(station1),concatenationString("&destinations=",concatenationString(latitude(station2),concatenationString(",",concatenationString(longitude(station2),"&mode=bicycling&language=fr-FR&key=AIzaSyCb7C7-gdJZt8XWFPVyLe2Kmg_k6cvXqDQ")))))))))
  result=xmlTreeParse(requete,useInternalNodes = TRUE)
  d=xmlValue(result[["//row//element/distance/value"]])
  return(strtoi(d))
}  

#Etape 2: vérifier que les stations proches sont vides/pleines (suivant le cas arrivée/départ) pour pouvoir les proposer

proposeAutreStationDep <- function(stationDepStr, heure.normalisee, jour.semaine.normalise, description.grade, temperature.grade, wind.grade,is.weekday,qualitytime_1, qualitytime_2, qualitytime_3, bikes_1, bikes_2, bikes_3, bikes_before, timediff){ #timeDep de la forme Y-M-D HH:MM:SS; on rentre la station sous le forme '00901-port-solferino-station-mobile', on retourne un string
  #fichier avec les stations les plus proches
  fichierDep=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/stations_proches/stations",stationDepStr),'.csv'))
  
  bikesDep=prediction_bikes(stationDepStr, heure.normalisee, jour.semaine.normalise, description.grade, temperature.grade, wind.grade,is.weekday,qualitytime_1, qualitytime_2, qualitytime_3, bikes_1, bikes_2, bikes_3, bikes_before, timediff)
  if(bikesDep<2){  #si la station de départ est vide, on propose une autre station, sinon on renvoie la station initiale
    cpt=2
    stationsProches=as.vector(fichierDep$station) #stationsProches[cpt] est le string de la station
    while(cpt<10){
      stationOpt=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/data_neighbors/paris/stations",stationProches[cpt]),'.csv'))
      bikesOpt=prediction_bikes(stationProches[cpt], heure.normalisee, jour.semaine.normalise, description.grade, temperature.grade, wind.grade,is.weekday, stationOpt$qualitytime_1, stationOpt$qualitytime_2, stationOpt$qualitytime_3, stationOpt$bikes_1, stationOpt$bikes_2, stationOpt$bikes_3, bikes_before, timediff)
      if(bikesOpt>1){  #on vérifie que la station proposée est disponible
        return(stationsProches[cpt])
        }
      cpt=cpt+1 #si ce n'est pas le cas, on prend une station un peu plus loin
      }
  }
return(stationDepStr)
}

proposeAutreStationDep('00906-gare-de-lest', 0.4876968, 0.6666667, 0.5, 0.1961003, 0.3009709, 1, 2.1500, 2.7333, 32.0000,1,0,0,1,10.833333)

proposeAutreStationArr <- function(stationArrStr, heure.normalisee.arr, jour.semaine.normalise, description.grade.arr, temperature.grade.arr, wind.grade.arr,is.weekday,qualitytime_1.arr, qualitytime_2.arr, qualitytime_3.arr, bikes_1.arr, bikes_2.arr, bikes_3.arr, bikes_before.arr, timediff.arr){ #on suppose que les données sont fournies; on rentre la station sous le forme '00901-port-solferino-station-mobile', on retourne un string
  #on récupère le temps de trajet via Google Maps
  #requete=getURL(concatenationString("https://maps.googleapis.com/maps/api/distancematrix/xml?origins=",concatenationString(latitude(stationDepStr),concatenationString(",",concatenationString(longitude(stationDepStr),concatenationString("&destinations=",concatenationString(latitude(stationArrStr),concatenationString(",",concatenationString(longitude(stationArrStr),"&mode=bicycling&language=fr-FR&key=AIzaSyCb7C7-gdJZt8XWFPVyLe2Kmg_k6cvXqDQ")))))))))
  #result=xmlTreeParse(requete,useInternalNodes = TRUE)
  #travelTime=strtoi(xmlValue(result[["//row//element/duration/value"]]))
  #timeArr=as.POSIXlt(timeDep)+travelTime
  
  #données
  stationArr=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/modified_data/paris/stations",stationArrStr),'.csv'))
  fichierArr=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/stations_proches/stations",stationArrStr),'.csv'))
  bikesMax=stationArr$bikes[1]+stationArr$spaces[1]
  bikesArr=prediction_bikes(stationArrStr, heure.normalisee.arr, jour.semaine.normalise, description.grade.arr, temperature.grade.arr, wind.grade.arr,is.weekday,qualitytime_1.arr, qualitytime_2.arr, qualitytime_3.arr, bikes_1.arr, bikes_2.arr, bikes_3.arr, bikes_before.arr, timediff.arr)
  
  if(bikesArr>bikesMax - 1){
    cpt=2
    stationsProches=as.vector(fichierArr$station)
    while(cpt<10){
      station=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/modified_data/paris/stations",stationsProches[cpt]),'.csv')) #fichier excel de la station
      bikesMax2=station$bikes[1]+station$spaces[1]
      bikesOpt=bikesArr=prediction_bikes(stationsProches[cpt], heure.normalisee.arr, jour.semaine.normalise, description.grade.arr, temperature.grade.arr, wind.grade.arr,is.weekday,qualitytime_1.arr, qualitytime_2.arr, qualitytime_3.arr, bikes_1.arr, bikes_2.arr, bikes_3.arr, bikes_before.arr, timediff.arr)
      if(bikesOpt<bikesMax2-1){  #on vérifie que la station proposée est disponible
        return(stationsProches[cpt])
      }
      cpt=cpt+1 #si ce n'est pas le cas, on prend une station un peu plus loin
    }
  }
  return(stationArrStr)
}

