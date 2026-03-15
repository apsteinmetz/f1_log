---
title: "f1_log"
format: html
---

## Instructions

Create a shiny app that allows users to track the progress of their Formula 1 season. 

The app should include the following features:

 A main page showing img/f1_logo.png at the top left and five tabs: 

### 1. "Races"
   
   1. the races tab should display the schedule for 2026 as separate cards for each race listing a country flag icon, country and date.   
   
   2.  A refresh button should be included to update the all data in all pages and tabs with the latest information from the API.
   
   3. Clicking on a race card will open a new page with four columns. the first column will show more details about the course and a map of the circuit. The second column will show the finish ranking for the qualifying and the third column will show the finish ranking for the race. The fourth column will show four highlight valueboxes, including the name of the driver and value of the Fastest Lap, Fastest Pit Stop, Most Overtakes and Penalties.  If the race is in the past fetch data from the api.  Show blank fields if the race is in the future.

### 2. "Predictions" 

    This tab has columns where:
   
   1. Users input their predictions for the full season rankings of drivers and teams.
   
   2. Users input their predictions for the Team and Driver with the "Most Podiums"
   
   3. Columns with the current rankings from the api are displayed next to the predictions so the user can see how well they are doing.
      
      Supply dropdown lists to select the driver and team in each column.  Restrict the dropdown list to drivers and teams that have not already been rank-predicted.  Supply a "Clear" Button to clear the predictions.

### 3. "Driver Points"
   
   1. Display  driver names in the left column and country flags along the top. Populate the table with the driver points from each race.  Leave cells blank if the race is in the future.

### 4. "Team Points" 
   
   1. Display team names in the left column and country flags along the top. Populate the table with the team points from each race. Leave cells blank if the race is in the future.

### 5. "About"
   
   1. Display, "Created by Art Steinmetz with Shiny for R from Posit" and the url for the data source.

## Coding Details

Use the tidyverse package for data manipulation and visualization, and the shiny package for building the interactive app. Use bslib and cards to create the shiny elements.

## Data Source

Fetch data about courses, races, drivers and teams from the endpoints in API documeted at [jolpica-f1/docs at main · jolpica/jolpica-f1 · GitHub](https://github.com/jolpica/jolpica-f1/tree/main/docs) Populate the app with data from the 2026 Formula 1 season.  
