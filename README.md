# Predicting-the-result-of-a-Game
<b>Datasets used:</b> RegularSeasonDetailedResults,TourneyCompactResults
<br><br>
<b>Description: </b>Predicted the results of NCAA tournament game using Regression Techniques. Steps in the project include Data Wrangling, cluster analysis on the season statistics and built a model that will predict how many points a team will score in its game in the tournament. The project was implemented in R and R Studio was used for this purpose. 
<ul>
<li>Performed data wrangling on “RegularSeasonDetailedResults” dataset to obtain statistics for each team in every season. </li>
  <li>Next, clustering was performed by scaling the preprocessed daatset from step 1 and applying KMeans algorithm to it. The optimal number of clusters to be used was found to be 8 using the elbow method. </li>
  <li> Applied  Linear Regression, Ridge, Lasso, Principal component Regression and K-Nearest Neighbor techniques to predict the score for each team in every tournament.  </li>
</ul>

<b>Conclusion:</b>
PCR had given the best results among all the regression techniques applied. 
