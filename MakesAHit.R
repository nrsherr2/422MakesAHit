# Group 05 - Spotify Hit Prediction Project

# Step 1: Read in data and perform PCA
spotify_rawdata <- read.csv("top2018.csv")

spotify_pca <- prcomp(spotify_rawdata[,c(4:15)], center=TRUE, scale=TRUE)
summary(spotify_pca)

# PCA summary seems to indicate PC1-6 are important