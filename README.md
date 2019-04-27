# Songs By The Numbers! Group 05

This is the repository that holds the work that we all did for this project. The explanation of our overall goals and results is best left to the final report that we have submitted, so here will instead focus on giving a breakdown of each of the files in this repo:

* ListOfGenres.txt = text file with all genres provided by Spotify (used for our python script to assign genre to each song)  
* MakesAHit.R = initial R file with a lot of the analysis that we did before pivoting our project. You can see we tried a multitude of methods and consistently got poor results which led to us changing our goal to focus on genre classification.  
* MakesAHit.py = python script for connecting to Spotify API, creating dataset to hold data from our playlist, cleaning the data, assigning genre and other data preprocessing tasks. This script creates a csv file of the playlist which we pass along to our new R file.  
* MakesAHit_nkaul2.R = here is our new R file which is where we import our dataset and create the random forest, perform hyperparameter tuning, and testing.  
* spotify_training_data.csv = the csv our python script creates which we pass to our R file for analysis
* cluster_hit.R = initial attempt at clustering songs
* genetic_clustering.R = R file containing script for novel genetic clustering approach for song recommendations!
