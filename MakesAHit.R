# Group 05 - Spotify Hit Prediction Project

# Step 1: Read in data and perform PCA
spotify_rawdata <- read.csv("top2018.csv")

spotify_pca <- prcomp(spotify_rawdata[,c(4:15)], center=TRUE, scale=TRUE)
summary(spotify_pca)
# PCA summary seems to indicate PC1-6 are important

# Creating a Scree plot to confirm
std_devs <- spotify_pca$sdev
vars <- std_devs^2
prop_var_explained <- vars / sum(vars) # variance explained by each PC

plot(prop_var_explained, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")
# Slope seems pretty consistent past PC3 but that explains only 44% variance, hmm...





# Get output per component
spotify_pca$rotation

# Graph all components 
biplot(spotify_pca)

