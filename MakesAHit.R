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
# Slope seems pretty consistent past PC3 but that explains only 44% variance

# Get output per component
spotify_pca$rotation

# Graph all components 
biplot(spotify_pca)

# PCA results seem inconclusive.
#---------------------------------------------------------------------------------------------------------------
# Trying stepwise variable selection

# adding rank in top 100 as an explicit column
spotify_rawdata$song_rank = c(1:100)
  
full_first_order_model = lm(song_rank ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness 
                                         + liveness + valence + tempo + duration_ms + time_signature, data=spotify_rawdata)

full_second_order_model = lm(song_rank ~ (danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness 
                         + liveness + valence + tempo + duration_ms + time_signature)^2 + I(danceability^2) + I(energy^2) +
                         I(loudness^2) + I(speechiness^2) + I(acousticness^2) + I(instrumentalness^2) + I(liveness^2) +
                         I(valence^2) + I(tempo^2) + I(duration_ms^2), data=spotify_rawdata)

output1 <- ols_step_both_p(full_first_order_model)
# the best 1st order model generated produces an R-squared value of 0.064

output2 <- ols_step_both_p(full_second_order_model)
# the best 2nd order model generated produces an R-squared value of 0.123

# Results from variable selection also seem to indicate a lack of a relationship between the attributes of a song and its popularity
#----------------------------------------------------------------------------------------------------------------


