songs <- read.csv("spotify_training_data.csv")
labels <- colnames(songs)
# install all necessary packages
required_packages = c("caret", "nnet", "stats", "dplyr", "ggplot2")
for(package in required_packages){
  if(!(package %in% installed.packages())){
    install.packages(package, dependencies = T)
  }   
}


# load the packages
library('caret') # for cross validation
library('nnet') # for neural networks
library('stats') # for clustering
library('dplyr') # if needed
library('ggplot2') # for plotting elbow plot


do_the_thing <- function(interest_columns,kclust){
  km <- cluster_from_specific_columns(columnNames = interest_columns,type="kmeans",addtodata = FALSE,nclust=kclust)
  sg <- cluster_from_specific_columns(columnNames = interest_columns,type="single",addtodata = FALSE,nclust=kclust)
  cp <- cluster_from_specific_columns(columnNames = interest_columns,nclust=kclust,addtodata = FALSE)
  if(length(interest_columns)==2){
    siiva <- songs[,interest_columns]
    plot(siiva,col=km)
  }
}


cluster_from_columns <- function(data=songs, leftColumn=1, rightColumn=2, type="complete", nclust=2, addtodata=FALSE){
  columnNames <- colnames(data)
  if(leftColumn>rightColumn || leftColumn<1 || rightColumn>length(columnNames)){
    print("invalid column numbers")
    return()
  }
  columnNames <- columnNames[leftColumn:rightColumn]
  
  siiva <- data[,columnNames]
  
  if(type == "single"){
    clusters <- cutree(hclust(d=dist(x=siiva, method="euclidean"),method="single"),k=nclust)
    if(addtodata==TRUE){
      return(add_to_data(clusters))
    }else{
      return(clusters)
    }
  } else if(type=="kmeans"){
    km <- kmeans(x=siiva, centers = nclust, iter.max = 100, algorithm = "Lloyd")$cluster
    if(addtodata==TRUE){
      return(add_to_data(km))
    }else{
      return(km)
    }
  }else{
    bonzai <- cutree(hclust(d=dist(x=siiva, method = "euclidean"), method = "complete"), k=nclust)
    if(addtodata==TRUE){
      return(add_to_data(bonzai))
    }else{
      return(bonzai)
    }
  }
}


cluster_from_specific_columns <- function(data=songs, columnNames=c("tempo"), type="complete", nclust=2, addtodata=FALSE){
  siiva <- data[,columnNames]
  
  if(type == "single"){
    clusters <- cutree(hclust(d=dist(x=siiva, method="euclidean"),method="single"),k=nclust)
    if(addtodata==TRUE){
      return(add_to_data(clusters))
    }else{
      return(clusters)
    }
  } else if(type=="kmeans"){
    km <- kmeans(x=siiva, centers = nclust, iter.max = 100, algorithm = "Lloyd")$cluster
    if(addtodata==TRUE){
      return(add_to_data(km))
    }else{
      return(km)
    }
  }else{
    bonzai <- cutree(hclust(d=dist(x=siiva, method = "euclidean"), method = "complete"), k=nclust)
    if(addtodata==TRUE){
      return(add_to_data(bonzai))
    }else{
      return(bonzai)
    }
  }
}

add_to_data <- function(assignments){
  rips <- songs
  rips$playlist <- assignments
  return(rips)
}

get_playlist_by_number <- function(altered_data,pl_num){
  return(altered_data[altered_data$playlist==pl_num,c("name","artist","genre","playlist")])
}

#do a big analysis for all combinations of variables, un-normalized first
for(i in 6:18){
  for(j in 6:18){
    if(i != j){
      for(k in 2:30){
       do_the_thing(c(labels[i],labels[j]),k)
      }
    }
  }
}



