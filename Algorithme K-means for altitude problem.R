path1="/Users/Администратор/Documents/1_Учеба/Ecole Polytechnique/2 год/PSC/optimisation/coordinates_toulouse.csv"
path2="/Users/Администратор/Documents/1_Учеба/Ecole Polytechnique/2 год/PSC/optimisation/coordinates_lyon.csv"
path3="/Users/Администратор/Documents/1_Учеба/Ecole Polytechnique/2 год/PSC/optimisation/coordinates_paris.csv"

datatoulouse=read.csv(path1)
datalyon=read.csv(path2)
dataparis=read.csv(path3)

altitudeparis=dataparis$altitude
altitudelyon=datalyon$altitude
altitudetoulouse=datatoulouse$altitude

clusters <- hclust(dist(altitudeparis))
plot(clusters)

#ces 3 variables contiennent les altitudes des stations de chaque ville

groupestoulouse=kmeans(altitudetoulouse,centers=3 ,nstart=20,iter.max = 30)
groupesparis=kmeans(altitudeparis,centers=3, nstart=20, iter.max =  30)
groupeslyon=kmeans(altitudelyon, centers =3 , nstart = 20, iter.max = 30)

groupestoulouse=as.data.frame(groupestoulouse[1])
groupeslyon=as.data.frame(groupeslyon[1])
groupesparis=as.data.frame(groupesparis[1])

#clusterCut <- cutree(clusters, 3)

#clusteraltitudelyon=data.frame(cluster=c(),stringsAsFactors = FALSE)
#clusteraltitudeparis=data.frame(cluster=c(),stringsAsFactors = FALSE)
#clusteraltitudetoulouse=data.frame(cluster=c(),stringsAsFactors = FALSE)

#on a crйe 3 clusters pour chaque ville selon les altitudes des diffйrentes stations
#on ajoute cette variable aux fichiers excel contenant les coordonnйes des stations 
#cette classification est ensuite utilisable pour nos prйdictions

datatoulouse=cbind(datatoulouse,groupestoulouse)
dataparis=cbind(dataparis,groupesparis)
datalyon=cbind(datalyon,groupeslyon)

#datatoulouse$groupestoulouse.cluster <- NULL
#datatoulouse$groupestoulouse.cluster.1 <- NULL
#datatoulouse$X.1 <- NULL
#datatoulouse$X <- NULL
#head(datatoulouse)

#datalyon$groupeslyon.cluster <- NULL
#datalyon$groupeslyon.cluster.1 <- NULL
#datalyon$X.1 <- NULL
#datalyon$X <- NULL
#head(datalyon)

#dataparis$groupesparis.cluster <- NULL
#dataparis$groupesparis.cluster.1 <- NULL
#dataparis$X.1 <- NULL
#dataparis$X <- NULL
#head(dataparis)

write.csv(datatoulouse,path1)
write.csv(dataparis,path3)
write.csv(datalyon,path2)


