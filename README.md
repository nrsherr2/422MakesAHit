# What Makes a Hit? Group 05

## Data Set
[Kaggle Dataset “Top Spotify Tracks of 2018”](https://www.kaggle.com/nadintamer/top-spotify-tracks-of-2018)

## Idea
What makes a song a hit? Artists have wanted for years to figure out that secret formula for getting to the top of the charts and integrating themselves into the cultural zeitgeist. Using data from the Spotify Web API, we hope to find out by applying data mining techniques learned in CSC 422. 

The data set includes features calculated by Spotify including (but not limited to): danceability, energy, key, and loudness. We believe that by analyzing the features of a variety of songs we can determine the most significant attributes that contribute to success. Discovering and training a novel model would allow us to ask questions to further our research such as: 
- Are certain artists more likely to create hits based on their style?
- Do lyrics matter when making a hit?
- What is the optimal length for a hit song?
- What factors (as judged by the Spotify Web API) are important in a song to be a hit?

## Software
We will first need to write software to read in the dataset and perform dimensionality reduction. Once we have reduced the number of features that we are concerned with, we will need to decide how to classify a “hit” either as a discrete amount of revenue or plays which must be surpassed or by some other measure. Finally, we will need to develop, train, tune, and test a model of our own design and see what new knowledge can be acquired. If we need more data, we can attempt to use data from prior years or more than just the top 100 of 2018.

## Papers
Academic works that we plan to use include:
- Dhanaraj, Ruth and Beth Logan. “Automatic Prediction of Hit Songs.” ISMIR (2005).
- Li, T. and Ogihara, M. and Tzanetakis, G. “Music Data Mining.” CRC Press (2011).
- Monterola, Christopher and Abundo, Cheryl and Tugaff, Jeric and Venturina, Lorcel Ericka. “Prediction of Potential Hit Song and Musical Genre Using Artificial Neural Networks.” International Journal of Modern Physics C (2009).

## Division of Labor
- Nick Sherrill will be in charge of technical issues, assisting the others in translating their algorithms and computational needs into code. 
- Nikhil Kaul will be in charge of analyzing the data using regression and classification trees, as well as visualization of data. 
- Valeri Kozarev will be in charge of analyzing the data set and performing dimensionality reduction in order to extract the most critical features of the data set.

## Midterm Milestone
We plan on having an untrained model by the midterm deadline. This leaves us enough time to perform hyperparameter tuning and improve our prediction accuracy as well as write a thorough report of our findings.
