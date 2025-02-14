# 📊 Football Data Analysis: Machine Learning, xG, Expected Points, Clustering, Tracking Data & Visualization

This project focuses on the analysis of football data, including the calculation of Expected Goals (xG), Expected Points (xP), and player clustering based on shooting precision and technique. The analysis covers different leagues and tournaments, such as the Dutch and Polish leagues, as well as data from the Women's and Men's World Cup 2023.

## 🚀 Introduction

These scripts perform the following steps:

- Load and preprocess football data from various sources (CSV, Excel, JSON).
- Calculate Expected Goals (xG) for the 2024 season.
- Calculate Expected Points (xP) for Brøndby IF based on their xG.
- Perform clustering on player performance based on shooting precision and technique.
- Visualize clustering results and key player statistics through Shiny apps and static plots.
- Analyze tracking data and event data from the Women's and Men's World Cup 2023.

## 📁 Contents

- **Eksamen – Datapreparation.R**: R script for loading and transforming football data in various formats (CSV, Excel, JSON).
- **Opgave 1.R**: R script for calculating Expected Goals (xG) for the 2024 football season.
- **Opgave 2.R**: R script for calculating Expected Points (xP) for Brøndby IF based on their xG and match simulations.
- **Opgave 3.R**: R script for clustering players based on shooting technique and precision in the Dutch and Polish leagues for the 2021/22 and 2022/23 seasons.
- **Opgave 4.R**: Shiny app for visualizing player data, clustering, and key statistics in the Dutch and Polish leagues.
- **Opgave 5.R**: R script for analyzing football data from the 2023 Women's and Men's World Cup, including shooting and passing analysis.
- **Opgave 6.R**: R script for analyzing player shots and goals, including visualizations of tracking data from MongoDB and coverage of ball positions.

## 📊 Data Analysis

### Eksamen – Datapreparation.R
- **Data Loading**:
- Loads multiple football datasets from different file formats such as RDS, Excel, CSV, and JSON.

- **Data Transformation**:
- Flattens JSON data for proper analysis and prepares data for further manipulation.

- **Data Structure**:
- Creates and indexes dataframes for easy analysis, ensuring compatibility across datasets.

### Opgave 1.R
- **Expected Goals Calculation (xG)**:
- Combines shot data and match information to calculate xG for the 2024 football season.

- **Data Cleaning**:
- Cleans shot-related data, removing irrelevant rows and filling in missing values.

- **Visualization**:
- Visualizes shot data, expected goals, and key statistics like shot angles and distances.

### Opgave 2.R
- **Expected Points Calculation (xP)**:
- Combines xG data with match results to calculate Expected Points (xP) for Brøndby IF based on their performance during the 2023/24 season.

- **Simulations**:
- Simulates match outcomes based on xG and number of shots taken, providing expected point values for the team.

- **Statistical Analysis**:
- Performs descriptive analysis on important variables such as xG, shots, and match statistics.

### Opgave 3.R
- **Clustering Analysis**:
- Applies K-means clustering to group players in the Dutch and Polish leagues based on their shooting technique and precision.

- **Feature Engineering**:
- Calculates average shot angles and expected goals (xG) for players in both leagues.

- **Cluster Evaluation**:
- Evaluates the clustering performance using inertia and visualizes cluster distributions with elbow plots.

- **Player Role Analysis**:
- Analyzes the player clusters with respect to their roles (strikers, defenders, midfielders).

### Opgave 4.R
- **Shiny App for Visualization**:
- An interactive Shiny app that allows users to explore player data, view clustering results, and compare player performances across leagues and seasons.

- **Features**:
- Users can select leagues and seasons, visualize player clustering, and view player statistics in scatter plots.
- Detailed descriptions of cluster characteristics, including average shot angles and xG.

### Opgave 5.R
- **World Cup 2023 Analysis**:
- Analyzes football data from the 2023 Women's and Men's World Cup, focusing on shots, passes, fouls, tackles, and cards.

- **Data Visualization**:
- Visualizes trends and differences between male and female players' performances.

- **Deep Dive into Shot Scenarios**:
- Analyzes shot data including freeze frames and coverage shadows.

### Opgave 6.R
- **Shots & Goals Analysis**:
- Identifies top scorers and top players based on shots and goals during the 2023 Women's World Cup.

- **Tracking Data**:
- Analyzes tracking data from MongoDB to visualize player movement and interactions.

- **Visualizing Ball Coverage**:
- Creates visual representations of ball coverage using triangular shadow overlays.

## 🛠️ Technologies

- **R**: The primary environment for data analysis and visualization.
- **dplyr, tidyr**: Libraries for data manipulation and transformation.
- **ggplot2**: Library for data visualization.
- **shiny**: Library for creating interactive web applications.
- **cluster**: Library for K-means clustering and silhouette analysis.
- **stats**: Library for performing statistical analyses.
- **jsonlite, readxl, jsonlite**: Libraries for loading and transforming JSON and Excel data.
- **MongoDB**: Used for tracking data storage and retrieval.

## 📌 Requirements

- R and RStudio must be installed on your machine.
- The following packages should be installed:

    ```r
    install.packages(c("dplyr", "tidyr", "ggplot2", "shiny", "cluster", "stats", "readxl", "jsonlite", "mongolite"))
    ```

## 🤝 Contributing

Contributions are welcome! If you have suggestions or improvements, feel free to fork the repository and submit a pull request.