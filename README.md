Look! It's your data!
======

## A shiny app for data visualization

This is a [Shiny](http://shiny.rstudio.com/) app that generates graphics used to explore your data set. 

Written in [R](http://cran.r-project.org/), this shiny app requires the following packages:

  * shiny
  * foreign (if you want to load SAS xport files)
  * ggplot2 
  * reshape2
  * RColorBrewer

## Description
If a picture is worth a thousand words, then how many tables are a single visualization worth? [Exploratory data analysis](http://en.wikipedia.org/wiki/Exploratory_data_analysis) is a great way to see what is and is not in your dataset. This app makes it easy to visualize your data quickly, without programming effort to get a jump on your [data wrangling](http://en.wikipedia.org/wiki/Data_wrangling).

This simple app reads in a data.frame from a csv or SAS xpt file, and generates a set of data visualizations. 

The first step is to classify the variables as continuous, logical or categorical. We classify any variable with only 2 unique values as logical. We have to use an ad-hoc definition for categorical variables as all factors plus all variables with more than 2 and less than 10 unique values. By default, character variables are converted to factors, however if we have more than 20 levels, we will not show a figure for that variable.

The app creates a faceted set of histograms for all categorical and logical variables, and another set of scatter plots for all continuous variables. Since I work in time-to-event settings most frequently, the app searches the variable names for a standard time related variable to use for the x-axis. Typically, we use a "date of procedure" for this. However, if your data does not have a "time" variable name, we will select the first continous variable for the x-axis. This variable can be changed though the Shiny interface.

A separate page is set up for visualizing individual variables, making it easy to export a single figure for use in reports. 

We still include a data summary for debugging purposes. 

## Running the App
The easiest way is to use the command to download the app from the [GitHub](https://github.com/ehrlinger/xportEDA) repository.
```
R> library(shiny)
R> runApp()
```

or run it directly from the repository:
```
R> iny::runGitHub("ehrlinger/xportEDA") 
```