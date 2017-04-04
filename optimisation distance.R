#Objectif: proposer une autre station de départ (respectivement d'arrivée) proche
#de la station de départ de l'utilisateur (respectivement d'arrivée) si celle-ci est vide (pleine) 

#critère de proximité: 250m
#critère de décision "station pleine/vide": nb de vélos prédits proche des bornes à 2 vélos près (ie 2 vélos ou bikesMax-2)

library(XML)
library(RCurl)
library(methods)

concatenationStringFile <- function(w1,w2){
  x<- c(w1,w2)
  y<- paste(x,sep="",collapse="/")
}
  
concatenationString <- function(w1,w2){
  x<- c(w1,w2)
  y<- paste(x,sep="",collapse="")
}

path="/Users/matthieurenard/Documents/X/PSC/data/paris/coordinates.csv"

stationtest1='00906-gare-de-lest'
stationtest2='00903-quai-mauriac-pont-de-bercy'
coordonnées=read.csv(path)
stations=coordonnées$station  #liste de string des noms de stations
l=length(stations)

#Etape 1: collecte des distances entre deux stations via l'API Google Maps et sélection des stations proches

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

distance(stationtest1,stationtest2)

'''rad2deg <- function(rad) {  #conversion radian vers degré
  (rad * 180) / (pi)
}

deg2rad <- function(deg) { #conversion degré vers radian
  (deg * pi) / (180)
}

distance <- function(station1, station2){  #distance entre station 1 et 2 en km
  for(i in 1:l){
    if(stations[i]==station1){  #on récupère les coordonnées des stations --> trop long, il faudrait avoir les coordonnées dans les fichiers
      lat1=coordonnées$latitude[i]
      lon1=coordonnées$longitude[i]
    }
    if(stations[i]==station2){
      lat2=coordonnées$latitude[i]
      lon2=coordonnées$longitude[i]
    }
  }
  
  d=111.13384*rad2deg(acos((sin(deg2rad(lat1))*sin(deg2rad(lat2))) + (cos(deg2rad(lat1))*cos(deg2rad(lat2))*cos(deg2rad(lon1-lon2)))))
  return(d)
}

distance(stationtest1,stationtest2)'''


"listeStationsProches <- function(stationStr){ #stations (en string) qui sont susceptibles d'être proposées; station est un string
  liste <- list()       
  
  for(i in 1:l){
    d=distance(stationStr, stations[i])
    if(d<0.25){
      vecteur=list(station=stations[i],distance=d)
      liste[[length(liste)+1]] <- vecteur
      }
    }
  
  
  if(len(liste)==0){   #si il n'y a aucune station à moins de 250m
    for(i in 1:l){
      d=distance(stationStr, stations[i])
      if(d<0.5){
        vecteur=list(station=stations[i],distance=d)
        liste[[length(liste)+1]] <- vecteur
      }
    }
  }
  
  stations <- sapply(liste,"[[","station")   #tri des stations selon leur distance
  distances <- sapply(liste,"[[","distance")
  stations[order(distance)]
  return(stations)  #tableau qui contient les strings des stations proches
}"

#listeStationsProches(stationtest1)

#Etape 2: vérifier que les stations proches sont vides/pleines (suivant le cas arrivée/départ) pour pouvoir les proposer

"timeArr <- function(timeDep, stationDepStr, stationArrStr){  #estime le temps d'arrivée à stationArr; timeDep de la forme Y-M-D HH:MM:SS; a voir: algo de P1
  d=distance(stationDepStr, stationArrStr)
  vitesseMoy=13/60   #vitesse en km par min
  timeDepReal= as.POSIXlt(timeDep)
  timeParcours= (d/vitesseMoy)*60  #en secondes
  timeArr=timeDepReal+timeParcours  #il faut maintenant modifier la date (changement de jour ou mois), fait automatiquement par le format POSIXlt
  return (timeArr)
}"


proposeAutreStationDep <- function(timeDep, stationDepStr){ #timeDep de la forme Y-M-D HH:MM:SS; on rentre la station sous le forme '00901-port-solferino-station-mobile', on retourne un string
  timeDepReal=as.POSIXct(timeDep)
  stationDep=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/modified_data/paris/stations",stationDepStr),'.csv')) #fichier excel de la station de départ
  fichierDep=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/stations_proches/stations",stationDepStr),'.csv'))
  for(i in 1:length(stationDep$moment)){
    if(as.POSIXct(stationDep$moment[i])<timeDepReal && timeDepReal<as.POSIXct(stationDep$moment[i+1])){  #on cherche un moment de relevé qui soit proche du temps de départ
      if(stationDep$bikes[i]<1 && stationDep$bikes[i+1]<1){  #si la station de départ est vide, on propose une autre station, sinon on renvoie la station initiale
        cpt=2
        stationsProches=as.vector(fichierDep$station) #stationsProches[cpt] est le string de la station
        while(cpt<10){
          print(stationsProches[cpt])
          station=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/modified_data/paris/stations",stationsProches[cpt]),'.csv')) #fichier excel de la station
          for(j in 1:length(station$moment)){
            if(as.POSIXct(station$moment[j])<timeDepReal && timeDepReal<as.POSIXct(station$moment[j+1])){
              if(station$bikes[j]>1 && station$bikes[j+1]>1){  #on vérifie que la station proposée est disponible
                return(stationsProches[cpt])
              }
            }
          }
          cpt=cpt+1 #si ce n'est pas le cas, on prend une station un peu plus loin
      }
     }
    }
  }
  return(stationDepStr)
}

proposeAutreStationDep("2016-04-04 22:58:30", '06005-montparnasse')

proposeAutreStationArr <- function(timeDep, stationDepStr, stationArrStr){ #timeDep de la forme Y-M-D HH:MM:SS; on rentre la station sous le forme '00901-port-solferino-station-mobile', on retourne un string
  requete=getURL(concatenationString("https://maps.googleapis.com/maps/api/distancematrix/xml?origins=",concatenationString(latitude(stationDepStr),concatenationString(",",concatenationString(longitude(stationDepStr),concatenationString("&destinations=",concatenationString(latitude(stationArrStr),concatenationString(",",concatenationString(longitude(stationArrStr),"&mode=bicycling&language=fr-FR&key=AIzaSyCb7C7-gdJZt8XWFPVyLe2Kmg_k6cvXqDQ")))))))))
  print(concatenationString("https://maps.googleapis.com/maps/api/distancematrix/xml?origins=",concatenationString(latitude(stationDepStr),concatenationString(",",concatenationString(longitude(stationDepStr),concatenationString("&destinations=",concatenationString(latitude(stationArrStr),concatenationString(",",concatenationString(longitude(stationArrStr),"&mode=bicycling&language=fr-FR&key=AIzaSyCb7C7-gdJZt8XWFPVyLe2Kmg_k6cvXqDQ")))))))))
  result=xmlTreeParse(requete,useInternalNodes = TRUE)
  travelTime=strtoi(xmlValue(result[["//row//element/duration/value"]])) #on récupère le temps de trajet via Google Maps
  timeArr=as.POSIXlt(timeDep)+travelTime
  stationArr=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/modified_data/paris/stations",stationArrStr),'.csv'))
  fichierArr=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/stations_proches/stations",stationArrStr),'.csv'))
  bikesMax=stationArr$bikes[1]+stationArr$spaces[1]
  for(i in 1:length(stationArr$moment)){
    if(as.POSIXct(stationArr$moment[i])<timeArr && timeArr<as.POSIXct(stationArr$moment[i+1])){
      if(stationArr$bikes[i]>bikesMax - 1 && stationArr$bikes[i+1]>bikesMax-1){
        cpt=2
        stationsProches=as.vector(fichierArr$station)
        while(cpt<10){
          station=read.csv(concatenationString(concatenationStringFile("/Users/matthieurenard/Documents/X/PSC/modified_data/paris/stations",stationsProches[cpt]),'.csv')) #fichier excel de la station
          bikesMax2=station$bikes[1]+station$spaces[1]
          for(j in 1:length(station$moment)){
            if(as.POSIXct(station$moment[j])<timeArr && timeArr<as.POSIXct(station$moment[j+1])){
              if(station$bikes[j]<bikesMax2-1 && station$bikes[j+1]<bikesMax2-1){  #on vérifie que la station proposée est disponible
                return(stationsProches[cpt])
              }
            }
          }
          cpt=cpt+1 #si ce n'est pas le cas, on prend une station un peu plus loin
        }
     }
   }
  }
  return(stationArrStr)
}

proposeAutreStationArr("2016-04-01 19:36:00", '04103-hotel-de-ville', '06107-cherche-midi')
  

