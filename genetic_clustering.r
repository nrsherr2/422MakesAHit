songs <- read.csv("spotify_training_data.csv",header=T)
songdata <- songs[6:18]

library("BBmisc")

gen_random_vector <- function(){
  return(sample(8:12, ncol(songdata),replace=T))
}
gen_seed_vectors <- function() {
  basevecs <- data.frame(matrix(NA, nrow=7,ncol=ncol(songdata)))
  for(i in 1:7){
    basevecs[i,]<-gen_random_vector()
  }
  return(basevecs)
}
next_gen <- function(userchoice, population){
  newpop <- population
  for(i in 2:(nrow(population-1))){
    for(j in 1:length(population[i,])){
      newpop[i,j] <- population[i-1,j] + population[i,j] + population[i+1,j] - 20
    }
  }
  newpop[1,] <- population[userchoice,]
  newpop[nrow(population),] <- gen_random_vector()
}
scale_with_weights <- function(weight_vector){
  scaled_data <- normalize(songdata,method="standardize")
  for(i in 1:length(weight_vector)){
    for(j in 1:length(scaled_data)){
      scaled_data[j,i] <- scaled_data[j,i]*(1/weight_vector[i])
    }
  }
  return(scaled_data)
}
genetic_algorithm <- function(weight_df, last_pick=8){
  print("starting new generation...")
  for(i in 1:nrow(weight_df)){
    vec <- weight_df[i,]
    scale_matr <- scale_with_weights(vec)
    #km <- kmeans(x=siiva, centers = nclust, iter.max = 100, algorithm = "Lloyd")$cluster
    assignments <- kmeans(x=scale_matr, centers=10, iter.max = 100, algorithm = "Lloyd")$cluster
    pl_to_save <- sample(1:10,1)
    fname <- paste(i,"playlist.csv",sep="")
    cpl <- songs[assignments==pl_to_save,2:4]
    write.csv(cpl,file=fname)
  }
  mypick <- readline(prompt="Read over the CSV files and pick the playlist you think is best: ")
  mypick <- as.integer(mypick)
  if(mypick == 1 && last_pick == 1){
    print("we have found a winning weight set")
    return(weight_df[1,])
  } else {
    return(genetic_algorithm(weight_df, mypick))
  }
}

vector_seeds <- gen_seed_vectors()
my_weights <- genetic_algorithm(vector_seeds)
